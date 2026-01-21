{ pkgs }:
{
  programs.waybar = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    systemd.enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 30;
        spacing = 4;

        modules-left = [
          "niri/workspaces"
          "niri/window"
        ];
        modules-center = [ "clock" ];
        modules-right = [
          "tray"
          "wireplumber"
          "network"
          "battery"
        ];

        "niri/workspaces" = {
          format = "{icon}";
          format-icons = {
            active = "";
            default = "";
          };
          on-click = "activate";
        };

        "niri/window" = {
          format = "{}";
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
      };
    };

    style = ''
      * {
        font-family: "HackGen Console NF", "Font Awesome 6 Free";
        font-size: 15px;
        min-height: 0;
      }

      window#waybar {
        background-color: rgba(40, 42, 54, 0.85);
        color: #f8f8f2;
        border-radius: 12px;
        margin: 8px 8px 0 8px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
      }

      #workspaces button {
        padding: 0 8px;
        color: #6272a4;
        background-color: transparent;
        border: none;
        border-radius: 0;
      }

      #workspaces button:hover {
        background-color: #44475a;
        color: #f8f8f2;
      }

      #workspaces button.active {
        color: #bd93f9;
      }

      #workspaces button.urgent {
        color: #ff5555;
      }

      #window {
        padding: 0 10px;
        color: #f8f8f2;
      }

      #clock {
        padding: 0 10px;
        color: #f8f8f2;
      }

      #battery {
        padding: 0 10px;
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
        padding: 0 10px;
        color: #8be9fd;
      }

      #network.disconnected {
        color: #ff5555;
      }

      #wireplumber {
        padding: 0 10px;
        color: #f1fa8c;
      }

      #wireplumber.muted {
        color: #6272a4;
      }

      #tray {
        padding: 0 10px;
      }

      #tray > .passive {
        -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: #ff5555;
      }

      tooltip {
        background-color: #282a36;
        border: 1px solid #bd93f9;
        border-radius: 8px;
      }

      tooltip label {
        color: #f8f8f2;
      }
    '';
  };
}
