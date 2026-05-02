{ pkgs }:
{
  programs.hyprlock = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    settings = {
      general = {
        disable_loading_bar = false;
        grace = 2;
        hide_cursor = true;
        no_fade_in = false;
        no_fade_out = false;
      };

      background = [
        {
          monitor = "";
          path = "screenshot";
          blur_passes = 3;
          blur_size = 8;
          noise = 0.0117;
          contrast = 0.8916;
          brightness = 0.8172;
          vibrancy = 0.1696;
          vibrancy_darkness = 0.0;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "300, 50";
          outline_thickness = 2;
          dots_size = 0.33;
          dots_spacing = 0.15;
          dots_center = true;
          dots_rounding = -1;
          outer_color = "rgb(BD93F9)";
          inner_color = "rgb(282A36)";
          font_color = "rgb(F8F8F2)";
          fade_on_empty = true;
          fade_timeout = 1000;
          placeholder_text = "<i>Password...</i>";
          hide_input = false;
          rounding = 8;
          check_color = "rgb(8BE9FD)";
          fail_color = "rgb(FF5555)";
          fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
          fail_transition = 300;
          capslock_color = "rgb(FFB86C)";
          numlock_color = -1;
          bothlock_color = -1;
          invert_numlock = false;
          swap_font_color = false;
          position = "0, -20";
          halign = "center";
          valign = "center";
        }
      ];

      label = [
        # Time
        {
          monitor = "";
          text = "$TIME";
          text_align = "center";
          color = "rgb(F8F8F2)";
          font_size = 90;
          font_family = "Inter";
          position = "0, 200";
          halign = "center";
          valign = "center";
        }
        # Date
        {
          monitor = "";
          text = "cmd[update:60000] date '+%A, %B %d'";
          text_align = "center";
          color = "rgb(BD93F9)";
          font_size = 24;
          font_family = "Inter";
          position = "0, 110";
          halign = "center";
          valign = "center";
        }
      ];
    };
  };
}
