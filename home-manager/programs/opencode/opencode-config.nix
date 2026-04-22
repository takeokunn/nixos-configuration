{
  pkgs,
  mcp-servers-nix,
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
    serena = {
      enable = true;
      context = "claude-code";
      enableWebDashboard = false;
    };
    context7.enable = true;
    playwright.enable = true;
  };

  settings = {
    theme = "dark";
    plugin = [ "oh-my-openagent" ];
    model = "openai/gpt-5.4";
    share = "disabled";

    provider.openai.options = providerTimeoutOpts;
    provider.anthropic.options = providerTimeoutOpts;
    provider."github-copilot".options = providerTimeoutOpts;

    compaction = {
      auto = true;
      prune = true;
      reserved = 10000;
    };

    watcher.ignore = [
      ".devenv/**"
      ".direnv/**"
      ".terraform/**"
      "result/**"
      ".git/**"
      "node_modules/**"
      "flake.lock"
    ];

    servers.deepwiki = {
      type = "http";
      url = "https://mcp.deepwiki.com/mcp";
    };

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
