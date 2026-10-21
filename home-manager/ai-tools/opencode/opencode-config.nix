{
  pkgs,
  mcp-servers-nix,
  nurPkgs,
  models,
}:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };
  sharedServers = shared.mcpServers { inherit nurPkgs; };

  # Upstream passes "--prefix PATH : ..." as one makeWrapperArgs element, which
  # makeWrapper rejects under structuredAttrs.
  astGrepMcp = nurPkgs.ast-grep-mcp.overrideAttrs (_: {
    makeWrapperArgs = [
      "--prefix"
      "PATH"
      ":"
      (lib.makeBinPath [ pkgs.ast-grep ])
    ];
  });

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
    model = models.default.model;
    small_model = models.default.model;
    share = "disabled";
    autoupdate = false;

    provider."opencode-go".options = providerTimeoutOpts;
    provider.openai.options = providerTimeoutOpts;
    provider.anthropic.options = providerTimeoutOpts;
    provider."github-copilot".options = providerTimeoutOpts;

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

    servers = lib.mapAttrs (_: shared.mcpServerToOpencode) sharedServers // {
      "ast-grep".type = "local";
      "ast-grep".command = [ "${astGrepMcp}/bin/ast-grep-server" ];
    };

    permission = {
      bash = {
        "*" = "allow";
      }
      // lib.genAttrs shared.bashDenyPatterns (_: "deny");
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
