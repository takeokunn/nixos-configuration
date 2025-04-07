{
  pkgs,
  config,
  mcp-servers-nix,
}:
let
  programs = {
    fetch.enable = true;
    playwright.enable = true;
    brave-search = {
      enable = true;
      envFile = ./.env;
    };
  };
  source = mcp-servers-nix.lib.mkConfig pkgs { inherit programs; };
in
[
  {
    # sops.templates.".env.brave-search".content = ''
    #   BRAVE_API_KEY="${config.sops.placeholder.brave-api-token}"
    # '';

    home.file."Library/Application\ Support/Claude/claude_desktop_config.json" = {
      inherit source;
    };
  }
]
