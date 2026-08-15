{ lib }:
# FR-008: single-source-of-truth for facts that were previously hand-copied across
# claude-code/default.nix, codex/default.nix, and opencode/{opencode-config,agent-translation}.nix
# and had already drifted (opencode missing several dangerous-Bash patterns; codex missing the
# metabase-mcp server). This file is a plain Nix library, not a Home Manager module: it is not
# listed in ai-tools/default.nix's `imports`, and each tool's own default.nix pulls in only the
# pieces it needs via `import ../shared { inherit lib; }`, the same way opencode/default.nix
# already imports opencode-config.nix and agent-translation.nix as plain expressions.
#
# opencode's Bash hook mechanism is JS/TS plugins, not shell scripts, so there is no
# opencode-side hook roster to unify here — (c) below is claude-code/codex only, on purpose.
rec {
  # (a) The three MCP servers that were hand-copied identically (or near-identically, in
  # codex's case missing metabase-mcp entirely) across all three tools. Every other server
  # (context7, playwright, slite, clickup, ast-grep, ...) differs per tool and stays declared
  # locally in that tool's own default.nix.
  #
  # Shape: a "local" (stdio) server is `{ command; args ? [ ]; }`; a remote server is
  # `{ type = "http"; url; }`. This is claude-code's and codex's native mcpServers shape
  # already (see the pre-existing `serena`/`deepwiki` blocks this replaces), so both consume
  # the result directly; opencode needs a small conversion, see `mcpServerToOpencode` below.
  mcpServers =
    { nurPkgs }:
    {
      serena = {
        command = "${nurPkgs.serena}/bin/serena";
        args = [
          "start-mcp-server"
          "--context"
          "claude-code"
          "--enable-web-dashboard"
          "false"
        ];
      };
      deepwiki = {
        type = "http";
        url = "https://mcp.deepwiki.com/mcp";
      };
      metabase-mcp = {
        command = "${nurPkgs.metabase-mcp}/bin/metabase-mcp";
      };
    };

  # Renders one server from `mcpServers` into the shape mcp-servers-nix's "opencode" flavor
  # expects: an explicit `type`, and `command` as a single argv list rather than separate
  # `command`/`args` fields.
  mcpServerToOpencode =
    def:
    if def ? url then
      {
        type = "http";
        url = def.url;
      }
    else
      {
        type = "local";
        command = [ def.command ] ++ (def.args or [ ]);
      };

  # (b) The union of claude-code's and opencode's previously independent dangerous-Bash deny
  # lists. Every pattern that existed in either tool's list is preserved here verbatim, even
  # where the two tools spelled the same hazard differently (Claude Code's own permission
  # syntax uses a trailing ":*" for "this prefix, then anything"; opencode's uses a bare "*").
  # Rather than picking one spelling, both are kept, so the union is a superset of both
  # originals and neither tool loses a rule it already had.
  #
  # claude-code wraps each entry as "Bash(<p>)"; opencode maps each to an attr `<p> = "deny"`.
  bashDenyPatterns = [
    "rm -rf /*"
    "rm -rf /"
    "sudo rm -:*"
    "sudo rm -rf *"
    "chmod 777 /*"
    "chmod -R 777 /*"
    "dd if=:*"
    "dd if=*"
    "mkfs.:*"
    "mkfs.*"
    "fdisk -:*"
    "fdisk *"
    "format -:*"
    "shutdown -:*"
    "shutdown *"
    "reboot -:*"
    "reboot *"
    "halt -:*"
    "halt *"
    "poweroff -:*"
    "poweroff *"
    "killall -:*"
    "killall *"
    "pkill -:*"
    "pkill -f *"
    "nc -l -:*"
    "ncat -l -:*"
    "netcat -l -:*"
    "rm -rf ~:*"
    "rm -rf $HOME:*"
    "rm -rf ~/.ssh*"
    "rm -rf ~/.config*"
  ];

  # opencode's permission.bash matcher (packages/opencode/src/util/wildcard.ts) escapes every
  # character except "*" before matching, so it reads ":" literally rather than as part of a
  # wildcard suffix. Claude Code's own permission syntax instead treats a trailing ":*" as
  # "this prefix, then anything" — Bash(<prefix>:*) — so any bashDenyPatterns entry spelled
  # that way (e.g. "fdisk -:*") renders in opencode as a glob requiring a literal trailing
  # colon that a real command will never contain, and the deny is silently inert. Stripping
  # the colon and keeping the bare "*" (opencode's own "match anything" spelling) restores the
  # intended reach. Entries with no trailing ":*" are already opencode-native and pass through
  # unchanged.
  bashDenyPatternToOpencode =
    p: if lib.hasSuffix ":*" p then (lib.removeSuffix ":*" p) + "*" else p;

  # bashDenyPatterns translated into opencode's spelling and de-duplicated: several translated
  # forms (e.g. "dd if=:*" -> "dd if=*") collide with a pattern that was already bare-star in
  # the shared list, and feeding a list with repeated entries into `lib.genAttrs` would define
  # the same attribute name twice.
  bashDenyPatternsOpencode = lib.unique (map bashDenyPatternToOpencode bashDenyPatterns);

  # (c) The guardrail hooks registered under a Bash matcher in settings.hooks.PreToolUse,
  # shared by claude-code and codex (opencode has no shell-hook mechanism to register these
  # into; its hooks are JS/TS plugins, out of scope for this change). rtk-rewrite is
  # deliberately absent from this list: claude-code installs it (see
  # claude-code/default.nix's `programs.claude-code.hooks.rtk-rewrite`) but leaves it
  # unregistered here because rtk is not on PATH for the harness, so every rewritten command
  # would exit 127 — that exemption and its full reasoning live as a comment beside
  # claude-code's own hook registration block. codex does not install rtk-rewrite at all
  # (`grep -n rtk home-manager/ai-tools/codex/default.nix` returns no matches), so for codex
  # there is nothing to exempt.
  guardrailHookNames = [
    "block-destructive-git"
    "block-bare-cd"
    "enforce-perl"
  ];

  # (d) One frontmatter parser for codex/default.nix and opencode/agent-translation.nix, which
  # both convert an ai-prompts command/agent markdown file (`---\nkey: value\n...\n---\nbody`)
  # into a tool-native format. Derives the body from the position of the CLOSING `---` rather
  # than a hardcoded `lib.drop 4 lines`: a hardcoded drop silently leaves a stray `---` in the
  # generated body for any frontmatter that is not exactly 2 lines long, while every assert in
  # either converter still passes, because neither ever re-checked the drop count against the
  # actual frontmatter length.
  parseFrontmatter =
    content:
    let
      lines = lib.splitString "\n" content;
      len = builtins.length lines;
      findClosing =
        i:
        if i >= len then
          null
        else if builtins.elemAt lines i == "---" then
          i
        else
          findClosing (i + 1);
      # Search starts at index 1: index 0 is the opening delimiter itself and must not match.
      closingIndex = findClosing 1;
    in
    assert lib.hasPrefix "---" (builtins.head lines);
    assert closingIndex != null;
    {
      frontmatterLines = lib.sublist 1 (closingIndex - 1) lines;
      body = lib.concatStringsSep "\n" (lib.drop (closingIndex + 1) lines);
    };

  # Finds the one frontmatter line starting with `prefix` (e.g. "name: ", "description: ").
  # Asserts it exists rather than returning null, so a malformed prompt file fails the Nix
  # build at eval time instead of shipping a tool config with a blank field.
  findLineWithPrefix =
    prefix: lines:
    let
      found = lib.findFirst (l: lib.hasPrefix prefix l) null lines;
    in
    assert found != null;
    found;
}
