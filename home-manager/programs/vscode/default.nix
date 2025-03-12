{ pkgs }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;

      extensions = with pkgs.vscode-extensions; [
        vscodevim.vim
        bbenoist.nix
        saoudrizwan.claude-dev
        editorconfig.editorconfig
        eamodio.gitlens
        tamasfe.even-better-toml
        oderwat.indent-rainbow
        mechatroner.rainbow-csv
      ];

      userSettings = {
        editor = {
          renderWhitespace = "boundary";
          minimap = {
            enabled = false;
            renderCharacters = false;
          };
        };
        vim = {
          showmodename = true;
          leader = "<space>";
          easymotion = true;
          easymotionKeys = "hklyuiopnm,qwertzxcvbasdgjf";
          easymotionMarkerBackgroundColor = "#E5E9F0";
          easymotionMarkerForegroundColorOneChar = "#5E81AC";
          easymotionMarkerForegroundColorTwoCharFirst = "#8FBCBB";
          easymotionMarkerForegroundColorTwoCharSecond = "#88C0D0";
          easymotionMarkerFontWeight = "bold";
          hlsearch = true;
          useSystemClipboard = true;
          visualstar = true;
        };
      };
    };
  };
}
