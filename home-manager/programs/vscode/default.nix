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
          leader = ",";
          easymotion = true;
          easymotionKeys = "hklyuiopnm,qwertzxcvbasdgjf";
          easymotionMarkerFontWeight = "bold";
          hlsearch = true;
          useSystemClipboard = true;
          visualstar = true;
        };
      };
    };
  };
}
