{ config, lib, ... }:
let
  palette = config.lib.stylix.colors;
  design = import ../../../shared/design.nix config.stylix.fonts;
  inherit (design) font size radius;
  pt = n: "${toString n}pt";
  px = n: "${toString n}px";
  colors = palette.withHashtag;
  rgba =
    base: alpha:
    "rgba(${palette."${base}-rgb-r"}, ${palette."${base}-rgb-g"}, ${palette."${base}-rgb-b"}, ${alpha})";

  glyph = points: icon: "<span font_family='${font.icons}' size='${pt points}'>${icon}</span>";
  # A rail chip: the glyph on top, a short value under it.
  chip = icon: value: "${glyph size.title icon}\n<span size='${pt size.caption}'>${value}</span>";
  clockLine =
    family: color: points: weight: text:
    "<span font_family='${family}' foreground='${colors.${color}}' size='${pt points}' weight='${weight}'>${text}</span>";

  # Workspaces not listed here (4-8 and any unnamed one) use base04.
  workspaceColors = {
    "1" = "base0C";
    "2" = "base0E";
    "3" = "base0B";
    "9" = "base0F";
    "10" = "base09";
  };
  # waybar names each niri button niri-workspace-<name>, and niri/default.nix names them 1-10.
  workspaceCss = selector: base: ''
    ${selector} {
      color: ${colors.${base}};
      background-color: ${rgba base "0.08"};
    }

    ${selector}:hover {
      background-color: ${rgba base "0.18"};
    }

    ${selector}.active {
      color: ${colors.base00};
      background-color: ${colors.${base}};
      box-shadow: 0 0 18px ${rgba base "0.55"};
    }

    ${selector}.urgent {
      color: ${colors.base08};
      background-color: ${rgba "base08" "0.2"};
    }
  '';
in
{
  programs.waybar.enable = true;
  programs.waybar.systemd.enable = true;
  # Stylix supplies the font and @define-color only; the style below keeps the layout.
  stylix.targets.waybar = {
    enable = true;
    addCss = false;
    font = "sansSerif";
  };

  programs.waybar.settings = {
    mainBar = {
      layer = "top";
      position = "left";
      width = 64;
      margin-top = 10;
      margin-bottom = 10;
      margin-left = 10;
      spacing = 6;

      modules-left = [ "niri/workspaces" ];
      modules-center = [ "clock" ];
      modules-right = [
        "cpu"
        "memory"
        "network"
        "wireplumber"
        "battery"
        "group/tray-drawer"
        "custom/power"
      ];

      "niri/workspaces" = {
        format = "{value}";
        on-click = "activate";
      };

      clock = {
        # One argument (the time) is formatted, so every placeholder names it by index.
        format = lib.concatStringsSep "\n" [
          (clockLine font.numeric "base0E" size.clock "bold" "{0:%H}")
          (clockLine font.numeric "base0C" size.clock "bold" "{0:%M}")
          (clockLine font.sans "base04" size.caption "normal" "{0:%m.%d}")
          (clockLine font.sans "base04" size.caption "normal" "{0:L%a}")
        ];
        locale = "en_US.UTF-8";
        justify = "center";
        tooltip-format = "<tt><small>{calendar}</small></tt>";
        calendar = {
          mode = "year";
          mode-mon-col = 3;
          weeks-pos = "right";
          format = {
            months = "<span color='${colors.base05}'><b>{}</b></span>";
            days = "<span color='${colors.base05}'>{}</span>";
            weeks = "<span color='${colors.base03}'>W{}</span>";
            weekdays = "<span color='${colors.base0F}'>{}</span>";
            today = "<span color='${colors.base0E}'><b><u>{}</u></b></span>";
          };
        };
      };

      cpu = {
        interval = 5;
        format = chip "󰍛" "{usage}%";
        justify = "center";
        states = {
          warning = 70;
          critical = 90;
        };
        tooltip-format = "CPU: {usage}%\nLoad: {load}";
      };

      memory = {
        interval = 10;
        format = chip "󰘚" "{used:0.1f}G";
        format-alt = chip "󰘚" "{percentage}%";
        justify = "center";
        states = {
          warning = 70;
          critical = 90;
        };
        tooltip-format = "RAM: {used:0.1f}G/{total:0.1f}G ({percentage}%)\nSwap: {swapUsed:0.1f}G/{swapTotal:0.1f}G ({swapPercentage}%)";
      };

      network = {
        format-wifi = glyph size.title "󰖩";
        format-ethernet = glyph size.title "󰈀";
        format-disconnected = glyph size.title "󰖪";
        justify = "center";
        tooltip-format = "{ifname}: {ipaddr}/{cidr}";
        tooltip-format-wifi = "{essid} {signalStrength}%\n{ifname}: {ipaddr}/{cidr}";
        on-click = "networkmanager_dmenu";
        on-click-right = "kitty --class bluetui -e bluetui";
      };

      wireplumber = {
        format = chip "{icon}" "{volume}";
        format-muted = chip "󰝟" "{volume}";
        format-icons = [
          "󰕿"
          "󰖀"
          "󰕾"
        ];
        justify = "center";
        on-click = "pavucontrol";
      };

      battery = {
        states = {
          warning = 30;
          critical = 15;
        };
        format = chip "{icon}" "{capacity}%";
        format-charging = chip "󰂄" "{capacity}%";
        format-plugged = chip "󰚥" "{capacity}%";
        format-icons = [
          "󰁺"
          "󰁻"
          "󰁼"
          "󰁽"
          "󰁾"
          "󰁿"
          "󰂀"
          "󰂁"
          "󰂂"
          "󰁹"
        ];
        justify = "center";
      };

      "group/tray-drawer" = {
        orientation = "inherit";
        drawer = {
          transition-duration = 300;
          transition-left-to-right = false;
        };
        modules = [
          "custom/tray-handle"
          "tray"
        ];
      };

      "custom/tray-handle" = {
        format = glyph size.subtitle "󰅃";
        tooltip = false;
      };

      tray = {
        spacing = 10;
      };

      "custom/power" = {
        format = glyph size.title "󰐥";
        tooltip = false;
        # The menu itself comes with the power-menu script.
        on-click = "power-menu";
      };
    };
  };

  programs.waybar.style = ''
    * {
      min-height: 0;
      transition: all 0.2s ease;
    }

    window#waybar {
      background-color: ${rgba "base00" "0.9"};
      color: ${colors.base05};
      border-radius: ${px radius.rail};
      border: 1px solid ${rgba "base05" "0.07"};
    }

    .modules-left {
      padding-top: 10px;
    }

    .modules-right {
      padding-bottom: 10px;
    }

    #workspaces button {
      min-width: 42px;
      min-height: 36px;
      padding: 0;
      margin: 3px 10px;
      border: none;
      border-radius: ${px radius.button};
      background-image: none;
      box-shadow: none;
      font-family: "${font.numeric}";
      font-size: ${pt size.label};
      font-weight: bold;
    }

    /* waybar packs the label at the start of the button, so it centers only at full width. */
    #workspaces button label {
      min-width: 42px;
    }

    ${workspaceCss "#workspaces button" "base04"}
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: base: workspaceCss "#workspaces button#niri-workspace-${name}" base
      ) workspaceColors
    )}

    #clock {
      padding: 8px 0;
    }

    #cpu,
    #memory,
    #network,
    #wireplumber,
    #battery,
    #custom-tray-handle {
      min-width: 46px;
      padding: 6px 0;
      margin: 0 8px;
      border-radius: ${px radius.chip};
    }

    #cpu {
      color: ${colors.base0C};
      background-color: ${rgba "base0C" "0.12"};
    }

    #memory {
      color: ${colors.base0F};
      background-color: ${rgba "base0F" "0.12"};
    }

    #network {
      color: ${colors.base0B};
      background-color: ${rgba "base0B" "0.12"};
    }

    #network.disconnected {
      color: ${colors.base08};
      background-color: ${rgba "base08" "0.12"};
    }

    #wireplumber {
      color: ${colors.base0A};
      background-color: ${rgba "base0A" "0.12"};
    }

    #wireplumber.muted {
      color: ${colors.base03};
      background-color: ${rgba "base03" "0.12"};
    }

    #battery {
      color: ${colors.base0B};
      background-color: ${rgba "base0B" "0.12"};
    }

    #battery.charging {
      color: ${colors.base0C};
      background-color: ${rgba "base0C" "0.12"};
    }

    #cpu.warning,
    #memory.warning,
    #battery.warning {
      color: ${colors.base09};
      background-color: ${rgba "base09" "0.12"};
    }

    #cpu.critical,
    #memory.critical,
    #battery.critical {
      color: ${colors.base08};
      background-color: ${rgba "base08" "0.13"};
      animation: pulse 1.5s ease-in-out infinite;
    }

    #custom-tray-handle {
      padding: 4px 0;
      color: ${colors.base04};
      background-color: ${rgba "base04" "0.08"};
    }

    #tray {
      padding: 4px 0;
    }

    #tray > .passive {
      -gtk-icon-effect: dim;
    }

    #tray > .needs-attention {
      -gtk-icon-effect: highlight;
    }

    #custom-power {
      min-width: 38px;
      min-height: 38px;
      margin: 4px 12px 0 12px;
      border-radius: 19px;
      color: ${colors.base08};
      background-color: ${rgba "base08" "0.14"};
    }

    @keyframes pulse {
      0% { opacity: 1; }
      50% { opacity: 0.5; }
      100% { opacity: 1; }
    }

    tooltip {
      background-color: ${rgba "base00" "0.95"};
      border: 1px solid ${colors.base0F};
      border-radius: ${px radius.chip};
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4);
    }

    tooltip label {
      color: ${colors.base05};
    }
  '';
}
