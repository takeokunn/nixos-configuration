{
  programs.k9s = {
    enable = true;
    settings.k9s = {
      noExitOnCtrlC = true;
      ui.skin = "dracula";
    };
    skins = {
      dracula =
        let
          foreground = "#f8f8f2";
          background = "#282a36";
          current_line = "#44475a";
          selection = "#44475a";
          comment = "#6272a4";
          cyan = "#8be9fd";
          green = "#50fa7b";
          orange = "#ffb86c";
          pink = "#ff79c6";
          purple = "#bd93f9";
          red = "#ff5555";
          yellow = "#f1fa8c";
        in
        {
          k9s = {
            body = {
              fgColor = foreground;
              bgColor = background;
              logoColor = purple;
            };
            prompt = {
              fgColor = foreground;
              bgColor = background;
              suggestColor = purple;
            };
            info = {
              fgColor = pink;
              sectionColor = foreground;
            };
            dialog = {
              fgColor = foreground;
              bgColor = background;
              buttonFgColor = foreground;
              buttonBgColor = purple;
              buttonFocusFgColor = yellow;
              buttonFocusBgColor = pink;
              labelFgColor = orange;
              fieldFgColor = foreground;
            };
            frame = {
              border = {
                fgColor = selection;
                focusColor = current_line;
              };
              menu = {
                fgColor = foreground;
                keyColor = pink;
                numKeyColor = pink;
              };
              crumbs = {
                fgColor = foreground;
                bgColor = current_line;
                activeColor = current_line;
              };
              status = {
                newColor = cyan;
                modifyColor = purple;
                addColor = green;
                errorColor = red;
                highlightColor = orange;
                killColor = comment;
                completedColor = comment;
              };
              title = {
                fgColor = foreground;
                bgColor = current_line;
                highlightColor = orange;
                counterColor = purple;
                filterColor = pink;
              };
            };
            views = {
              charts = {
                bgColor = "default";
                defaultDialColors = [
                  purple
                  red
                ];
                defaultChartColors = [
                  purple
                  red
                ];
              };
              table = {
                fgColor = foreground;
                bgColor = background;
                header = {
                  fgColor = foreground;
                  bgColor = background;
                  sorterColor = cyan;
                };
              };
              xray = {
                fgColor = foreground;
                bgColor = background;
                cursorColor = current_line;
                graphicColor = purple;
                showIcons = false;
              };
              yaml = {
                keyColor = pink;
                colonColor = purple;
                valueColor = foreground;
              };
              logs = {
                fgColor = foreground;
                bgColor = background;
                indicator = {
                  fgColor = foreground;
                  bgColor = purple;
                  toggleOnColor = green;
                  toggleOffColor = cyan;
                };
              };
            };
          };
        };
    };
  };
}
