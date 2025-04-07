{ pkgs, mcp-servers-nix }:
let
  programs = {
    fetch.enable = true;
    playwright.enable = true;
  };
in
[
  {
    home.file."Library/Application\ Support/Claude/claude_desktop_config.json" = {
      source = mcp-servers-nix.lib.mkConfig pkgs { inherit programs; };
    };
  }
]
