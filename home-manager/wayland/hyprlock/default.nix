{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
let
  colors = config.lib.stylix.colors;
  font = config.stylix.fonts.sansSerif.name;
  clockFont = "Geist Mono";
  date = "${pkgs.coreutils}/bin/date";

  batteryStatus = pkgs.writeShellScript "hyprlock-battery" ''
    for battery in /sys/class/power_supply/BAT*; do
      [ -r "$battery/capacity" ] || continue
      if [ "$(${pkgs.coreutils}/bin/cat "$battery/status")" = Charging ]; then
        icon=󰂄
      else
        icon=󰁹
      fi
      printf '%s %s%%\n' "$icon" "$(${pkgs.coreutils}/bin/cat "$battery/capacity")"
      exit 0
    done
  '';
  # Prints nothing when no player is running; the label is parsed as Pango markup.
  nowPlaying = pkgs.writeShellScript "hyprlock-now-playing" ''
    track="$(${pkgs.playerctl}/bin/playerctl metadata --format '{{ artist }} - {{ title }}' 2>/dev/null)" || exit 0
    [ -n "$track" ] || exit 0
    printf '󰎆 %s\n' "$(printf '%s' "$track" | ${pkgs.gnused}/bin/sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g')"
  '';

  topLeft = {
    monitor = "";
    halign = "left";
    valign = "top";
  };
  bottomRight = {
    monitor = "";
    halign = "right";
    valign = "bottom";
  };
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
      blur_passes = 2;
      blur_size = 8;
      noise = 0.0117;
      contrast = 0.8916;
      brightness = 0.6;
      vibrancy = 0.1696;
      vibrancy_darkness = 0.0;
    };

    input-field = topLeft // {
      size = "360, 56";
      outline_thickness = 2;
      # outer_color takes a gradient; Stylix sets a single color.
      outer_color = lib.mkForce "rgb(${colors.base0C}) rgb(${colors.base0F}) rgb(${colors.base0E}) 45deg";
      dots_size = 0.33;
      dots_spacing = 0.15;
      dots_center = true;
      dots_rounding = -1;
      fade_on_empty = true;
      fade_timeout = 1000;
      placeholder_text = "<i>Password...</i>";
      hide_input = false;
      rounding = -1;
      fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
      capslock_color = "rgb(${colors.base09})";
      numlock_color = -1;
      bothlock_color = -1;
      invert_numlock = false;
      swap_font_color = false;
      font_family = font;
      position = "140, -840";
    };

    label =
      map (label: topLeft // label) [
        {
          text = ''cmd[update:1000] echo "<b>$(${date} +%H)</b>"'';
          color = "rgb(${colors.base0E})";
          font_size = 200;
          font_family = clockFont;
          position = "140, -180";
        }
        {
          text = ''cmd[update:1000] echo "<b>$(${date} +%M)</b>"'';
          color = "rgb(${colors.base0C})";
          font_size = 200;
          font_family = clockFont;
          position = "140, -420";
        }
        {
          text = "cmd[update:60000] LC_TIME=ja_JP.UTF-8 ${date} '+%-m月%-d日 %A'";
          color = "rgb(${colors.base05})";
          font_size = 30;
          font_family = font;
          position = "140, -720";
        }
        {
          text = "${osConfig.networking.hostName} · ${config.home.username}";
          color = "rgb(${colors.base04})";
          font_size = 16;
          font_family = font;
          position = "140, -780";
        }
        {
          text = "󰌆";
          color = "rgb(${colors.base0E})";
          font_size = 16;
          font_family = "Symbols Nerd Font";
          position = "524, -856";
        }
        {
          text = "または YubiKey にタッチ";
          color = "rgb(${colors.base04})";
          font_size = 14;
          font_family = font;
          position = "556, -856";
        }
      ]
      ++ map (label: bottomRight // label) [
        {
          text = "cmd[update:60000] ${batteryStatus}";
          color = "rgb(${colors.base0B})";
          font_size = 16;
          font_family = font;
          position = "-48, 48";
        }
        {
          text = "cmd[update:5000] ${nowPlaying}";
          color = "rgb(${colors.base04})";
          font_size = 14;
          font_family = font;
          position = "-48, 84";
        }
      ];
  };
}
