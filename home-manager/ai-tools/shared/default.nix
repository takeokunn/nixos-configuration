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

  # Handed to Claude Code as NODE_EXTRA_CA_CERTS from two places, the claude wrapper
  # (../claude-code/default.nix) and the managed-settings env (shared/claude-code-managed-settings.nix),
  # which reach different launches; the managed-settings comment says why both are needed.
  claudeCodeCaBundle = "/etc/ssl/certs/ca-certificates.crt";

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

  # Plain `*` globs only: Claude Code reads `x:*` as `x*`, and opencode has no `:*` form, so one
  # spelling serves both. An entry another one already covers is omitted (`rm -rf ~*` covers
  # `rm -rf ~/.ssh*`).
  #
  # git is denied outright: agents use jj for every VCS operation. These prefixes miss `/usr/bin/git`,
  # `env git`, and `bash -c 'git ...'`; block-destructive-git catches those for Claude Code and Codex,
  # and nothing does for opencode, which runs no hooks.
  #
  # gh alias and gh extension run arbitrary programs under a gh command line, and gh auth token and
  # gh auth status -t print the token into the transcript.
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
    "git"
    "git *"
    "aitools git"
    "aitools git *"
    "gh alias *"
    "gh extension *"
    "gh ext *"
    "gh auth token*"
    "gh auth status *-t*"
    "gh auth status *--show-token*"
  ];

  # The catalog of guardrail scripts, not the wiring. Both Codex and Claude Code wire every name here.
  # block-destructive-git kept its name after it grew to block every git invocation, not only the
  # destructive ones.
  guardrailHookNames = [
    "block-destructive-git"
    "block-bare-cd"
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
