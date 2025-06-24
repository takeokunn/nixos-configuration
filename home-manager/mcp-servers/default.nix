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
      enable = true;
      envFile = config.sops.secrets.brave-api-token.path;
    };
  };
  settings = {
    servers.sitemcp-takeokunn-org = {
      command = "${nodePkgs."sitemcp"}/bin/sitemcp";
      args = [
        "https://www.takeokunn.org/"
      ];
    };
  };
in
[
  {
    home.file."Library/Application\ Support/Claude/claude_desktop_config.json" = {
      source = mcp-servers-nix.lib.mkConfig pkgs {
        inherit programs settings;
      };
    };
  }
]
