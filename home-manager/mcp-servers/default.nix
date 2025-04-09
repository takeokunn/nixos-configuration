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
  settings = {
    servers.sitemcp = {
      command = "${pkgs.lib.getExe' pkgs.nodejs "npx"}";
      args = [ "sitemcp" ];
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
