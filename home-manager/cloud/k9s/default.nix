{ config, ... }:
{
  programs.k9s.enable = true;
  programs.k9s.settings.k9s.noExitOnCtrlC = true;
  programs.k9s.settings.k9s.ui.skin = "dracula";
  programs.k9s.skins.dracula =
    let
      colors = config.lib.stylix.colors.withHashtag;
      foreground = colors.base05;
      background = colors.base00;
      current_line = colors.base02;
      selection = colors.base02;
      comment = colors.base03;
      cyan = colors.base0C;
      green = colors.base0B;
      orange = colors.base09;
      pink = colors.base0E;
      purple = colors.base0F;
      red = colors.base08;
      yellow = colors.base0A;
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
}
