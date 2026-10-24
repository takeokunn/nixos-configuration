{ lib }:
rec {
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
    };

  # Handed to Claude Code as NODE_EXTRA_CA_CERTS from two places: the claude wrapper
  # (home-manager/ai-tools/claude-code/default.nix) and the managed-settings env
  # (shared/claude-code-managed-settings.nix at the repository root).
  claudeCodeCaBundle = "/etc/ssl/certs/ca-certificates.crt";

  mcpServerToOpencode =
    def:
    if def ? url then
      {
        type = "remote";
        url = def.url;
      }
    else
      {
        type = "local";
        command = [ def.command ] ++ (def.args or [ ]);
      };

  # Plain `*` globs only: Claude Code reads `x:*` as `x*`, and opencode has no `:*` form, so one
  # spelling serves both. An entry another one already covers is omitted (`rm -rf ~*` covers
  # `rm -rf ~/.ssh*`).
  #
  # git writes are denied and read-only git is not: agents use jj for every VCS write. The write list
  # below is the only git guard for opencode, which runs no hooks. Its prefixes miss `/usr/bin/git`,
  # `env git`, and `bash -c 'git ...'`; block-destructive-git catches those for Claude Code and Codex
  # by allowing only read-only subcommands, which a prefix list cannot express. Some writes are also
  # not expressible as a prefix because the same words begin a read: `git tag v1`, `git config k v`,
  # and `git branch x` share a prefix with `git tag -l`, `git config --get`, and `git branch --list`.
  # block-destructive-git covers them for Claude Code and Codex, and opencode's `git *` ask default
  # covers them there.
  #
  # gh alias and gh extension run arbitrary programs under a gh command line, and gh auth token and
  # gh auth status -t print the token into the transcript.
  gitWriteSubcommands = [
    "commit"
    "commit-tree"
    "push"
    "reset"
    "checkout"
    "switch"
    "rebase"
    "merge"
    "stash push"
    "stash pop"
    "stash apply"
    "stash drop"
    "stash clear"
    "clean"
    "restore"
    "add"
    "rm"
    "mv"
    "pull"
    "fetch"
    "clone"
    "init"
    "am"
    "apply"
    "cherry-pick"
    "revert"
    "tag -d"
    "tag -a"
    "tag -f"
    "tag --delete"
    "branch -d"
    "branch -D"
    "branch -m"
    "branch -M"
    "branch -f"
    "branch -c"
    "branch --delete"
    "update-ref"
    "gc"
    "prune"
    "bisect"
    "submodule"
    "filter-branch"
    "reflog expire"
    "reflog delete"
    "worktree add"
    "worktree remove"
    "worktree move"
    "worktree prune"
    "remote add"
    "remote remove"
    "remote set-url"
    "config --add"
    "config --unset"
    "notes"
    "symbolic-ref"
    "update-index"
    "read-tree"
    "hash-object"
    "sparse-checkout"
    "checkout-index"
    "format-patch"
    "archive"
  ];

  # Subcommands that never write. opencode allows them ahead of its `git *` ask default, and the
  # Codex rule conversion uses them to drop a deny prefix that would also forbid a read.
  gitReadSubcommands = [
    "status"
    "log"
    "diff"
    "show"
    "blame"
    "rev-parse"
    "ls-files"
    "ls-tree"
    "ls-remote"
    "merge-base"
    "rev-list"
    "cat-file"
    "grep"
    "describe"
    "shortlog"
    "name-rev"
    "for-each-ref"
    "show-ref"
    "count-objects"
    "check-ignore"
    "remote -v"
    "remote show"
    "remote get-url"
    "config --get"
    "config --get-all"
    "branch --list"
    "branch --show-current"
    "tag -l"
    "tag --list"
    "stash list"
    "worktree list"
    "--version"
  ];

  # A bare `git stash` pushes, and `git push` with no arguments pushes the current branch, so each
  # subcommand is denied with and without arguments. The `-C dir` and `-c k=v` forms are the global
  # options agents add most; Claude Code and opencode match `*` at any position, Codex lists the
  # non-prefix forms as unenforced.
  gitWriteDenyPatterns = [
    "git stash"
  ]
  ++ lib.concatMap (sub: [
    "git ${sub}"
    "git ${sub} *"
    "git -C * ${sub}"
    "git -C * ${sub} *"
    "git -c * ${sub}"
    "git -c * ${sub} *"
  ]) gitWriteSubcommands;

  bashDenyPatterns = [
    "rm -rf /"
    "rm -rf /*"
    "rm -rf ~*"
    "rm -rf $HOME*"
    "sudo rm -*"
    "chmod 777 /*"
    "chmod -R 777 /*"
    "dd if=*"
    "mkfs.*"
    "fdisk *"
    "format -*"
    "shutdown *"
    "reboot *"
    "halt *"
    "poweroff *"
    "killall *"
    "pkill -*"
    "nc -l -*"
    "ncat -l -*"
    "netcat -l -*"
    "gh alias *"
    "gh extension *"
    "gh ext *"
    "gh auth token*"
    "gh auth status *-t*"
    "gh auth status *--show-token*"
  ]
  ++ gitWriteDenyPatterns;

  # The catalog of guardrail scripts, not the wiring. Both Codex and Claude Code wire every name here.
  guardrailHookNames = [
    "block-destructive-git"
  ];

  agentIsReadOnly =
    frontmatterLines:
    let
      tools =
        prefix:
        let
          line = lib.findFirst (lib.hasPrefix prefix) null frontmatterLines;
        in
        if line == null then null else map lib.trim (lib.splitString "," (lib.removePrefix prefix line));
      allowed = tools "tools:";
      denied = tools "disallowedTools:";
    in
    lib.all
      (
        tool:
        (allowed != null && !(builtins.elem tool allowed)) || (denied != null && builtins.elem tool denied)
      )
      [
        "Write"
        "Edit"
        "NotebookEdit"
      ];

  agentOpencodePermissions =
    frontmatterLines:
    lib.optionalString (agentIsReadOnly frontmatterLines) ''
      permission:
        '*': deny
        read: allow
        glob: allow
        grep: allow
    '';

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
      closingIndex = findClosing 1;
    in
    assert lib.hasPrefix "---" (builtins.head lines);
    assert closingIndex != null;
    {
      frontmatterLines = lib.sublist 1 (closingIndex - 1) lines;
      body = lib.concatStringsSep "\n" (lib.drop (closingIndex + 1) lines);
    };

  decodeFrontmatterScalar =
    value: if lib.hasPrefix "\"" value then builtins.fromJSON value else value;

  findLineWithPrefix =
    prefix: lines:
    let
      found = lib.findFirst (l: lib.hasPrefix prefix l) null lines;
    in
    assert found != null;
    found;
}
