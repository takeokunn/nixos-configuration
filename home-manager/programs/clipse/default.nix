{ pkgs }:
{
  home.packages = with pkgs; pkgs.lib.optionals pkgs.stdenv.isLinux [
    clipse
    cliphist
  ];

  xdg.configFile."clipse/config.json" = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    text = builtins.toJSON {
      historyFile = "clipboard_history.json";
      maxHistory = 500;
      allowDuplicates = false;
      tempDir = "/tmp/clipse";
      logFile = "clipse.log";

      # Vi-style keybindings
      keyBindings = {
        choose = "enter";
        clearSelected = "S";
        down = "j";
        end = "G";
        filter = "/";
        home = "g";
        more = "?";
        nextPage = "ctrl+f";
        page = "ctrl+d";
        pin = "p";
        prevPage = "ctrl+b";
        preview = "tab";
        quit = "q";
        remove = "x";
        selectDown = "J";
        selectSingle = "s";
        selectUp = "K";
        togglePin = "P";
        up = "k";
        yank = "y";
      };

      # Dracula theme
      theme = {
        useCustomTheme = true;
        DimmedDesc = "#6272a4";
        DimmedTitle = "#6272a4";
        FilteredMatch = "#ff79c6";
        NormalDesc = "#f8f8f2";
        NormalTitle = "#bd93f9";
        SelectedDesc = "#f8f8f2";
        SelectedTitle = "#50fa7b";
        SelectedBorder = "#bd93f9";
        SelectedDescBorder = "#44475a";
        TitleFore = "#f8f8f2";
        Titleback = "#282a36";
        StatusMsg = "#8be9fd";
        PinIndicatorColor = "#f1fa8c";
        SelectedBorderRadius = 1;
        UnselectedBorderRadius = 0;
        PreviewBorderRadius = 1;
      };

      imageDisplay = {
        type = "basic";
        scaleX = 9;
        scaleY = 9;
      };
    };
  };
}
