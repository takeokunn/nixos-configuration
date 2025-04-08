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
      envFile = config.sops.secrets.brave-api-token.path;
    };
  };
  source = mcp-servers-nix.lib.mkConfig pkgs { inherit programs; };
in
[
  {
    home.file."Library/Application\ Support/Claude/claude_desktop_config.json" = {
      inherit source;
    };
  }
]
