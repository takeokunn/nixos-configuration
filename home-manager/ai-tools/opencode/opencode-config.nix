{
  pkgs,
  mcp-servers-nix,
  nurPkgs,
}:
let
  providerTimeoutOpts = {
    timeout = 600000;
    chunkTimeout = 60000;
  };
in
mcp-servers-nix.lib.mkConfig pkgs {
  flavor = "opencode";
  fileName = "opencode.json";

  programs = {
    context7.enable = true;
    playwright.enable = true;
  };

  settings = {
    theme = "dark";
    plugin = [ "oh-my-openagent" ];
    model = "openai/gpt-5.5";
    share = "disabled";

    provider.openai.options = providerTimeoutOpts;
    provider.anthropic.options = providerTimeoutOpts;
    provider."github-copilot".options = providerTimeoutOpts;
    provider."zai-coding-plan".options = providerTimeoutOpts;

    compaction.auto = true;
    compaction.prune = true;
    compaction.reserved = 10000;

    watcher.ignore = [
      ".devenv/**"
      ".direnv/**"
      ".terraform/**"
      "result/**"
      ".git/**"
      "node_modules/**"
      "flake.lock"
    ];

    servers.serena.type = "local";
    servers.serena.command = [
      "${nurPkgs.serena}/bin/serena"
      "start-mcp-server"
      "--context"
      "claude-code"
      "--enable-web-dashboard"
      "false"
    ];

    servers.deepwiki.type = "http";
    servers.deepwiki.url = "https://mcp.deepwiki.com/mcp";

    servers."ast-grep".type = "local";
    servers."ast-grep".command = [ "${nurPkgs.ast-grep-mcp}/bin/ast-grep-server" ];

    servers.metabase-mcp.type = "local";
    servers.metabase-mcp.command = [ "${nurPkgs.metabase-mcp}/bin/metabase-mcp" ];

    permission = {
      bash = {
        "*" = "allow";
        "rm -rf /*" = "deny";
        "rm -rf /" = "deny";
        "sudo rm -rf *" = "deny";
        "dd if=*" = "deny";
        "mkfs.*" = "deny";
        "fdisk *" = "deny";
        "shutdown *" = "deny";
        "reboot *" = "deny";
        "halt *" = "deny";
        "poweroff *" = "deny";
        "killall *" = "deny";
        "pkill -f *" = "deny";
      };
      edit = "allow";
      write = "allow";
      read = "allow";
      glob = "allow";
      grep = "allow";
      webfetch = "allow";
      search = "allow";
      ask = "allow";
      memo = "allow";
      http = "allow";
    };
  };
}
