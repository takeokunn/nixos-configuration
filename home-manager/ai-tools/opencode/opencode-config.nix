{
  pkgs,
  mcp-servers-nix,
  nurPkgs,
}:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };
  sharedServers = shared.mcpServers { inherit nurPkgs; };

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
    model = "opencode-go/deepseek-v4-pro";
    small_model = "opencode-go/deepseek-v4-flash";
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

    # FR-008: serena/deepwiki/metabase-mcp come from shared/default.nix, converted into
    # mcp-servers-nix's "opencode" flavor shape (explicit type, single argv command list).
    # ast-grep stays opencode-local, matching claude-code and codex each declaring their own
    # extra servers.
    servers = lib.mapAttrs (_: shared.mcpServerToOpencode) sharedServers // {
      "ast-grep".type = "local";
      "ast-grep".command = [ "${nurPkgs.ast-grep-mcp}/bin/ast-grep-server" ];
    };

    permission = {
      # FR-008: the raw pattern list is shared/default.nix's bashDenyPatterns (the union of
      # this list and claude-code's own, see that file), run through
      # `shared.bashDenyPatternToOpencode` first because opencode's matcher reads a literal
      # ":" rather than expanding claude-code's trailing ":*" spelling — see that function's
      # comment. Each translated, de-duplicated pattern becomes a `<p> = "deny"` attr here;
      # "*" = "allow" stays as the explicit default this permission set had before.
      bash = {
        "*" = "allow";
      }
      // lib.genAttrs shared.bashDenyPatternsOpencode (_: "deny");
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
