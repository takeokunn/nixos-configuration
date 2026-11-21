{ config, ... }:
let
  colors = config.lib.stylix.colors;
  font = config.stylix.fonts.sansSerif.name;
in
{
  programs.hyprlock.enable = true;
  # Stylix sets the wallpaper and the input-field colors.
  stylix.targets.hyprlock.enable = true;
  programs.hyprlock.settings = {
    general = {
      disable_loading_bar = false;
      grace = 2;
      hide_cursor = true;
    };

    # A single attrset (not a list) so it merges with the path and colors the Stylix target sets.
    background = {
      monitor = "";
      blur_passes = 3;
      blur_size = 8;
      noise = 0.0117;
      contrast = 0.8916;
      brightness = 0.8172;
      vibrancy = 0.1696;
      vibrancy_darkness = 0.0;
    };

    input-field = {
      monitor = "";
      size = "300, 50";
      outline_thickness = 2;
      dots_size = 0.33;
      dots_spacing = 0.15;
      dots_center = true;
      dots_rounding = -1;
      fade_on_empty = true;
      fade_timeout = 1000;
      placeholder_text = "<i>Password...</i>";
      hide_input = false;
      rounding = 8;
      fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
      capslock_color = "rgb(${colors.base09})";
      numlock_color = -1;
      bothlock_color = -1;
      invert_numlock = false;
      swap_font_color = false;
      position = "0, -20";
      halign = "center";
      valign = "center";
    };

    label = [
      {
        monitor = "";
        text = "$TIME";
        text_align = "center";
        color = "rgb(${colors.base05})";
        font_size = 90;
        font_family = font;
        position = "0, 200";
        halign = "center";
        valign = "center";
      }
      {
        monitor = "";
        text = "cmd[update:60000] date '+%A, %B %d'";
        text_align = "center";
        color = "rgb(${colors.base0F})";
        font_size = 24;
        font_family = font;
        position = "0, 110";
        halign = "center";
        valign = "center";
      }
    ];
  };
}
