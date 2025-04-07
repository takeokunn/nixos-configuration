{ pkgs, mcp-servers-nix }:
[
  {
    home.file."Library/Application\ Support/Claude/claude_desktop_config.json".source =
      mcp-servers-nix.lib.mkConfig pkgs
        {
          programs = {
            fetch.enable = true;
            playwright.enable = true;
          };
        };
  }
]
