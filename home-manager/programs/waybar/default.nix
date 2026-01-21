{ pkgs }:
{
  programs.waybar = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    systemd.enable = true;

    settings = {
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
          "cpu"
          "memory"
          "disk"
          "custom/weather"
          "mpris"
          "tray"
          "wireplumber"
          "network"
          "battery"
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
              months = "<span color='#f8f8f2'><b>{}</b></span>";
              days = "<span color='#f8f8f2'>{}</span>";
              weeks = "<span color='#6272a4'>W{}</span>";
              weekdays = "<span color='#bd93f9'>{}</span>";
              today = "<span color='#ff79c6'><b><u>{}</u></b></span>";
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
          on-click = "kitty -e nmtui";
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

        tray = {
          spacing = 10;
        };

        cpu = {
          interval = 5;
          format = "󰍛 {usage}%";
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
          states = {
            warning = 70;
            critical = 90;
          };
          tooltip-format = "RAM: {used:0.1f}G/{total:0.1f}G ({percentage}%)\nSwap: {swapUsed:0.1f}G/{swapTotal:0.1f}G ({swapPercentage}%)";
        };

        disk = {
          path = "/";
          interval = 30;
          format = " {percentage_used}%";
          format-alt = " {free}";
          states = {
            warning = 75;
            critical = 90;
          };
          tooltip-format = "Disk: {used} / {total} ({percentage_used}%)";
        };

        "custom/weather" = {
          exec = "curl -s 'https://wttr.in/Tokyo?format=%c+%t' 2>/dev/null || echo '--'";
          interval = 1800;
          format = "{}";
          tooltip = false;
        };

        mpris = {
          format = "{player_icon} {artist} - {title}";
          format-paused = "{status_icon} {artist} - {title}";
          player-icons = {
            default = "";
            spotify = "";
            firefox = "";
            mpv = "🎵";
          };
          status-icons = {
            paused = "⏸";
            stopped = "⏹";
          };
          max-length = 40;
        };
      };
    };

    style = ''
      * {
        font-family: "HackGen Console NF", "Font Awesome 7 Free";
        font-size: 17px;
        min-height: 0;
        transition: all 0.2s ease;
      }

      window#waybar {
        background-color: rgba(40, 42, 54, 0.75);
        color: #f8f8f2;
        border-radius: 16px;
        border: 1px solid rgba(248, 248, 242, 0.1);
        margin: 8px 12px 0 12px;
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4);
      }

      #workspaces button {
        padding: 0 8px;
        color: #6272a4;
        background-color: transparent;
        border: none;
        border-radius: 8px;
        margin: 4px 2px;
      }

      #workspaces button:hover {
        background-color: rgba(68, 71, 90, 0.9);
        color: #f8f8f2;
      }

      #workspaces button.active {
        color: #bd93f9;
        background-color: rgba(189, 147, 249, 0.2);
      }

      #workspaces button.urgent {
        color: #ff5555;
        background-color: rgba(255, 85, 85, 0.2);
      }

      #window {
        padding: 0 12px;
        color: #f8f8f2;
      }

      #clock {
        padding: 0 12px;
        color: #f8f8f2;
        font-weight: bold;
      }

      #cpu {
        padding: 0 12px;
        color: #8be9fd;
      }

      #cpu.warning {
        color: #ffb86c;
      }

      #cpu.critical {
        color: #ff5555;
      }

      #memory {
        padding: 0 12px;
        color: #bd93f9;
      }

      #memory.warning {
        color: #ffb86c;
      }

      #memory.critical {
        color: #ff5555;
      }

      #disk {
        padding: 0 12px;
        color: #50fa7b;
      }

      #disk.warning {
        color: #ffb86c;
      }

      #disk.critical {
        color: #ff5555;
      }

      #custom-weather {
        padding: 0 12px;
        color: #f1fa8c;
      }

      #mpris {
        padding: 0 12px;
        color: #ff79c6;
      }

      #mpris.playing {
        color: #50fa7b;
      }

      #mpris.paused {
        color: #6272a4;
      }

      #battery {
        padding: 0 12px;
        color: #50fa7b;
      }

      #battery.warning {
        color: #ffb86c;
      }

      #battery.critical {
        color: #ff5555;
      }

      #battery.charging {
        color: #8be9fd;
      }

      #network {
        padding: 0 12px;
        color: #8be9fd;
      }

      #network.disconnected {
        color: #ff5555;
      }

      #wireplumber {
        padding: 0 12px;
        color: #f1fa8c;
      }

      #wireplumber.muted {
        color: #6272a4;
      }

      #tray {
        padding: 0 12px;
      }

      #tray > .passive {
        -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: #ff5555;
      }

      tooltip {
        background-color: rgba(40, 42, 54, 0.95);
        border: 1px solid #bd93f9;
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4);
      }

      tooltip label {
        color: #f8f8f2;
      }
    '';
  };
}
