{ config, pkgs, ... }:
let
  palette = config.lib.stylix.colors;
  colors = palette.withHashtag;
  rgba =
    base: alpha:
    "rgba(${palette."${base}-rgb-r"}, ${palette."${base}-rgb-g"}, ${palette."${base}-rgb-b"}, ${alpha})";

  tailscaleStatus = pkgs.writeShellScript "waybar-tailscale" ''
    if ${pkgs.tailscale}/bin/tailscale status --json 2>/dev/null | ${pkgs.jq}/bin/jq -e '.BackendState == "Running"' >/dev/null; then
      printf '{"text":"󰖂","class":"connected","tooltip":"Tailscale: connected"}\n'
    else
      printf '{"text":"󰖂","class":"disconnected","tooltip":"Tailscale: disconnected"}\n'
    fi
  '';
  # Each prints nothing while inactive, which hides the module.
  recordingStatus = pkgs.writeShellScript "waybar-recording" ''
    if ${pkgs.procps}/bin/pgrep -f wl-screenrec >/dev/null; then
      printf '{"text":"%s","class":"recording","tooltip":"Recording: Mod+Shift+R stops it"}\n' "󰑊"
    fi
  '';
  doNotDisturbStatus = pkgs.writeShellScript "waybar-do-not-disturb" ''
    if ${pkgs.mako}/bin/makoctl mode | ${pkgs.gnugrep}/bin/grep -qx do-not-disturb; then
      printf '{"text":"%s","class":"on","tooltip":"Do not disturb: Mod+Alt+N turns it off"}\n' "󰂛"
    fi
  '';
  tailscaleToggle = pkgs.writeShellScript "waybar-tailscale-toggle" ''
    if ${pkgs.tailscale}/bin/tailscale status >/dev/null 2>&1; then
      exec ${pkgs.tailscale}/bin/tailscale down
    fi
    exec ${pkgs.tailscale}/bin/tailscale up
  '';
in
{
  programs.waybar.enable = true;
  programs.waybar.systemd.enable = true;

  programs.waybar.settings = {
    mainBar = {
      layer = "top";
      position = "top";
      height = 40;
      spacing = 8;

      modules-left = [
        "niri/workspaces"
        "niri/window"
      ];
      modules-center = [ "clock" ];
      modules-right = [
        "custom/recording"
        "privacy"
        "custom/do-not-disturb"
        "cpu"
        "memory"
        "network"
        "bluetooth"
        "wireplumber"
        "battery"
        "power-profiles-daemon"
        "idle_inhibitor"
        "custom/tailscale"
        "tray"
      ];

      "niri/workspaces" = {
        format = "{icon}";
        format-icons = {
          active = "";
          default = "";
        };
        on-click = "activate";
      };

      "niri/window" = {
        format = "󰖯 {}";
        max-length = 50;
      };

      clock = {
        format = "{:%H:%M}";
        format-alt = "{:%Y-%m-%d %H:%M}";
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

      battery = {
        states = {
          warning = 30;
          critical = 15;
        };
        format = "{icon} {capacity}%";
        format-charging = "󰂄 {capacity}%";
        format-plugged = "󰚥 {capacity}%";
        format-warning = "󰁺⚠ {capacity}%";
        format-critical = "󰁺🔥 {capacity}%";
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
      };

      network = {
        format-wifi = "󰖩 {signalStrength}%";
        format-ethernet = "󰈀";
        format-disconnected = "󰖪";
        tooltip-format = "{ifname}: {ipaddr}/{cidr}";
        on-click = "networkmanager_dmenu";
      };

      wireplumber = {
        format = "{icon} {volume}%";
        format-muted = "󰝟";
        format-icons = [
          "󰕿"
          "󰖀"
          "󰕾"
        ];
        on-click = "pavucontrol";
      };

      bluetooth = {
        format = "󰂯";
        format-disabled = "󰂲";
        format-connected = "󰂱 {num_connections}";
        tooltip-format-connected = "{device_enumerate}";
        tooltip-format-enumerate-connected = "{device_alias}";
        on-click = "kitty --class bluetui -e bluetui";
      };

      power-profiles-daemon = {
        format = "{icon}";
        tooltip-format = "Power profile: {profile}";
        format-icons = {
          default = "󰗑";
          performance = "󰓅";
          balanced = "󰾅";
          power-saver = "󰾆";
        };
      };

      # Mac's sleep_prevent item: holds an idle/sleep inhibitor while active.
      idle_inhibitor = {
        format = "{icon}";
        format-icons = {
          activated = "󰅶";
          deactivated = "󰾪";
        };
      };

      "custom/recording" = {
        exec = "${recordingStatus}";
        return-type = "json";
        interval = 2;
      };

      "custom/do-not-disturb" = {
        exec = "${doNotDisturbStatus}";
        return-type = "json";
        interval = 2;
      };

      "custom/tailscale" = {
        exec = "${tailscaleStatus}";
        return-type = "json";
        interval = 10;
        on-click = "${tailscaleToggle}";
      };

      tray = {
        spacing = 10;
      };

      cpu = {
        interval = 5;
        format = "󰍛 {usage}%";
        format-warning = "󰍛⚠ {usage}%";
        format-critical = "󰍛🔥 {usage}%";
        format-icons = [
          "▁"
          "▂"
          "▃"
          "▄"
          "▅"
          "▆"
          "▇"
          "█"
        ];
        states = {
          warning = 70;
          critical = 90;
        };
        tooltip-format = "CPU: {usage}%\nLoad: {load}";
      };

      memory = {
        interval = 10;
        format = "󰘚 {used:0.1f}G/{total:0.1f}G";
        format-alt = "󰘚 {percentage}%";
        format-warning = "󰘚⚠ {used:0.1f}G/{total:0.1f}G";
        format-critical = "󰘚🔥 {used:0.1f}G/{total:0.1f}G";
        states = {
          warning = 70;
          critical = 90;
        };
        tooltip-format = "RAM: {used:0.1f}G/{total:0.1f}G ({percentage}%)\nSwap: {swapUsed:0.1f}G/{swapTotal:0.1f}G ({swapPercentage}%)";
      };

    };
  };

  programs.waybar.style = ''
    * {
      font-family: "${config.stylix.fonts.monospace.name}", "Font Awesome 7 Free";
      font-size: 17px;
      min-height: 0;
      transition: all 0.2s ease;
    }

    window#waybar {
      background-color: ${rgba "base00" "0.75"};
      color: ${colors.base05};
      border-radius: 16px;
      border: 1px solid ${rgba "base05" "0.1"};
      margin: 8px 12px 0 12px;
      box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4);
    }

    #workspaces button {
      padding: 0 8px;
      color: ${colors.base03};
      background-color: transparent;
      border: none;
      border-radius: 8px;
      margin: 4px 2px;
    }

    #workspaces button:hover {
      background-color: ${rgba "base02" "0.9"};
      color: ${colors.base05};
    }

    #workspaces button.active {
      color: ${colors.base0F};
      background-color: ${rgba "base0F" "0.2"};
    }

    #workspaces button.urgent {
      color: ${colors.base08};
      background-color: ${rgba "base08" "0.2"};
    }

    #window,
    #clock,
    #cpu,
    #memory,
    #battery,
    #network,
    #wireplumber,
    #tray {
      padding: 0 12px;
    }

    #window {
      color: ${colors.base05};
    }

    #clock {
      color: ${colors.base05};
      font-weight: bold;
    }

    #cpu {
      color: ${colors.base0C};
    }

    #memory {
      color: ${colors.base0F};
    }

    #battery {
      color: ${colors.base0B};
    }

    #cpu.warning,
    #memory.warning,
    #battery.warning {
      color: ${colors.base09};
      text-shadow: 0 0 8px ${rgba "base09" "0.8"};
    }

    #cpu.critical,
    #memory.critical,
    #battery.critical {
      color: ${colors.base08};
      text-shadow: 0 0 10px ${rgba "base08" "1.0"};
      animation: pulse 1.5s ease-in-out infinite;
    }

    #battery.charging {
      color: ${colors.base0C};
    }

    #network {
      color: ${colors.base0C};
    }

    #network.disconnected {
      color: ${colors.base08};
    }

    #wireplumber {
      color: ${colors.base0A};
    }

    #wireplumber.muted {
      color: ${colors.base03};
    }

    #bluetooth,
    #power-profiles-daemon,
    #idle_inhibitor {
      color: ${colors.base0C};
    }

    #idle_inhibitor.activated,
    #power-profiles-daemon.performance {
      color: ${colors.base09};
    }

    #custom-recording,
    #privacy {
      padding: 0 12px;
      color: ${colors.base08};
    }

    #custom-do-not-disturb {
      padding: 0 12px;
      color: ${colors.base09};
    }

    #custom-tailscale.connected {
      color: ${colors.base0B};
    }

    #custom-tailscale.disconnected {
      color: ${colors.base03};
    }

    #tray > .passive {
      -gtk-icon-effect: dim;
    }

    #tray > .needs-attention {
      -gtk-icon-effect: highlight;
    }

    @keyframes pulse {
      0% { opacity: 1; }
      50% { opacity: 0.5; }
      100% { opacity: 1; }
    }

    tooltip {
      background-color: ${rgba "base00" "0.95"};
      border: 1px solid ${colors.base0F};
      border-radius: 12px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4);
    }

    tooltip label {
      color: ${colors.base05};
    }
  '';
}
