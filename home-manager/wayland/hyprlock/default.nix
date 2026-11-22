{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
let
  colors = config.lib.stylix.colors;
  inherit (import ../../../shared/design.nix config.stylix.fonts)
    font
    size
    date
    ;
  # Geist ExtraBold with tabular figures: the mockup's heavy digits.
  clockDigits =
    format:
    ''cmd[update:1000] echo "<span weight='800' font_features='${font.figures}'>$(${dateBin} +${format})</span>"'';
  dateBin = "${pkgs.coreutils}/bin/date";

  # The rail's battery chip in Pango markup: the same level icons and the same colours (cyan while
  # charging, orange at 30% and red at 15% on battery), the glyph from the icon font.
  batteryStatus = pkgs.writeShellScript "hyprlock-battery" ''
    icons=(󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹)
    for battery in /sys/class/power_supply/BAT*; do
      [ -r "$battery/capacity" ] || continue
      capacity=$(${pkgs.coreutils}/bin/cat "$battery/capacity")
      level=$((capacity / 10))
      ((level > 9)) && level=9
      icon=''${icons[level]}
      color=${colors.base0B}
      case "$(${pkgs.coreutils}/bin/cat "$battery/status")" in
        Charging) icon=󰂄 color=${colors.base0C} ;;
        Full | "Not charging") icon=󰚥 ;;
        *)
          if ((capacity <= 15)); then
            color=${colors.base08}
          elif ((capacity <= 30)); then
            color=${colors.base09}
          fi
          ;;
      esac
      printf "<span font_family='%s' size='%s' foreground='#%s'>%s</span>  <span weight='600' foreground='#%s'>%s%%</span>\n" \
        "${font.icons}" "${toString size.title}pt" "$color" "$icon" "$color" "$capacity"
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
      blur_size = 3;
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
      # Always shown, as in the mockup; an empty, faded field hides where to type.
      fade_on_empty = false;
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
      font_family = font.sans;
      position = "140, -840";
    };

    label =
      map (label: topLeft // label) [
        {
          text = clockDigits "%H";
          color = "rgb(${colors.base0E})";
          font_size = size.lockClock;
          font_family = font.sans;
          position = "140, -180";
        }
        {
          text = clockDigits "%M";
          color = "rgb(${colors.base0C})";
          font_size = size.lockClock;
          font_family = font.sans;
          position = "140, -420";
        }
        {
          # LC_ALL, not LC_TIME: the session exports LC_ALL, which overrides LC_TIME.
          text = ''cmd[update:60000] echo "<b>$(LC_ALL=${date.locale}.UTF-8 ${dateBin} '+${date.format}')</b>"'';
          color = "rgb(${colors.base05})";
          font_size = size.heading;
          font_family = font.sans;
          position = "140, -720";
        }
        {
          text = "${osConfig.networking.hostName} · ${config.home.username}";
          color = "rgb(${colors.base04})";
          font_size = size.title;
          font_family = font.sans;
          position = "140, -780";
        }
        {
          text = "󰌆";
          color = "rgb(${colors.base0E})";
          font_size = size.title;
          font_family = font.icons;
          position = "524, -856";
        }
        {
          text = "or touch your YubiKey";
          color = "rgb(${colors.base04})";
          font_size = size.subtitle;
          font_family = font.sans;
          position = "556, -856";
        }
      ]
      ++ map (label: bottomRight // label) [
        {
          text = "cmd[update:60000] ${batteryStatus}";
          color = "rgb(${colors.base05})";
          font_size = size.subtitle;
          font_family = font.sans;
          position = "-48, 48";
        }
        {
          text = "cmd[update:5000] ${nowPlaying}";
          color = "rgb(${colors.base04})";
          font_size = size.subtitle;
          font_family = font.sans;
          position = "-48, 84";
        }
      ];
  };
}
