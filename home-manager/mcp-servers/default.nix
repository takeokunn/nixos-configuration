{
  pkgs,
  config,
  mcp-servers-nix,
  nodePkgs,
}:
let
  programs = {
    # fetch.enable = true;
    playwright.enable = true;
    brave-search = {
      enable = false;
    };
  };
in
[
  {
    home.file."Library/Application\ Support/Claude/claude_desktop_config.json" = {
      source = mcp-servers-nix.lib.mkConfig pkgs {
        inherit programs;
      };
    };
  }
]
