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
      # Auth is the Claude-Code-only /design-login and /design-sync slash commands, not the generic
      # remote-MCP OAuth flow deepwiki/Slite/ClickUp use, so this entry is inert in opencode and Codex
      # even though it's registered there too. Do not copy this entry's shared placement as precedent
      # for a server that isn't similarly Claude-Code-gated.
      "claude-design" = {
        type = "http";
        url = "https://api.anthropic.com/v1/design/mcp";
      };
    };

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

  bashDenyPatternToOpencode = p: if lib.hasSuffix ":*" p then (lib.removeSuffix ":*" p) + "*" else p;

  bashDenyPatternsOpencode = lib.unique (map bashDenyPatternToOpencode bashDenyPatterns);

  # The catalog of guardrail scripts, not the wiring. Codex wires all three; Claude Code wires the
  # first two and covers enforce-perl through guard-and-guide's rules.toml instead. Removing
  # enforce-perl here to match Claude Code would disarm Codex, which runs danger-full-access and
  # has no guard-and-guide agent mode available to it.
  guardrailHookNames = [
    "block-destructive-git"
    "block-bare-cd"
    "enforce-perl"
  ];

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

  findLineWithPrefix =
    prefix: lines:
    let
      found = lib.findFirst (l: lib.hasPrefix prefix l) null lines;
    in
    assert found != null;
    found;
}
