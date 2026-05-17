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
        "cpu"
        "memory"
        "disk"
        "custom/weather"
        "tray"
        "backlight"
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

      backlight = {
        format = "{icon} {percent}%";
        format-icons = [
          "󰃞"
          "󰃟"
          "󰃠"
        ];
        tooltip-format = "Brightness: {percent}%\n\nScroll: Adjust brightness\nClick: Brightness 100%\nRight-click: Brightness 50%\nMiddle-click: Brightness 10%";
        on-scroll-up = "swayosd-client --brightness raise";
        on-scroll-down = "swayosd-client --brightness lower";
        on-click = "swayosd-client --brightness +100";
        on-click-right = "swayosd-client --brightness 50";
        on-click-middle = "swayosd-client --brightness 10";
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

      disk = {
        path = "/";
        interval = 30;
        format = " {percentage_used}%";
        format-alt = " {free}";
        format-warning = "⚠ {percentage_used}%";
        format-critical = "🔥 {percentage_used}%";
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
    };
  };

  programs.waybar.style = ''
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
      text-shadow: 0 0 8px rgba(255, 184, 108, 0.8);
    }

    #cpu.critical {
      color: #ff5555;
      text-shadow: 0 0 10px rgba(255, 85, 85, 1.0);
      animation: pulse 1.5s ease-in-out infinite;
    }

    #memory {
      padding: 0 12px;
      color: #bd93f9;
    }

    #memory.warning {
      color: #ffb86c;
      text-shadow: 0 0 8px rgba(255, 184, 108, 0.8);
    }

    #memory.critical {
      color: #ff5555;
      text-shadow: 0 0 10px rgba(255, 85, 85, 1.0);
      animation: pulse 1.5s ease-in-out infinite;
    }

    #disk {
      padding: 0 12px;
      color: #50fa7b;
    }

    #disk.warning {
      color: #ffb86c;
      text-shadow: 0 0 8px rgba(255, 184, 108, 0.8);
    }

    #disk.critical {
      color: #ff5555;
      text-shadow: 0 0 10px rgba(255, 85, 85, 1.0);
      animation: pulse 1.5s ease-in-out infinite;
    }

    #custom-weather {
      padding: 0 12px;
      color: #f1fa8c;
    }

    #battery {
      padding: 0 12px;
      color: #50fa7b;
    }

    #battery.warning {
      color: #ffb86c;
      text-shadow: 0 0 8px rgba(255, 184, 108, 0.8);
    }

    #battery.critical {
      color: #ff5555;
      text-shadow: 0 0 10px rgba(255, 85, 85, 1.0);
      animation: pulse 1.5s ease-in-out infinite;
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

    #backlight {
      padding: 0 12px;
      color: #f1fa8c;
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
    }

    @keyframes pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.5; }
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
}
