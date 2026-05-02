{ pkgs }:
{
  services.hypridle = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    settings = {
      general = {
        lock_cmd = "pidof hyprlock || hyprlock";
        before_sleep_cmd = "loginctl lock-session";
        after_sleep_cmd = "command -v niri >/dev/null && niri msg action power-on-monitors || true";
        ignore_dbus_inhibit = false;
        ignore_systemd_inhibit = false;
      };

      listener = [
        # Dim screen after 5 minutes
        {
          timeout = 300;
          on-timeout = "brightnessctl -s set 30%";
          on-resume = "brightnessctl -r";
        }
        # Lock screen after 10 minutes
        {
          timeout = 600;
          on-timeout = "pidof hyprlock || hyprlock";
        }
        # Turn off screen after 15 minutes
        {
          timeout = 900;
          on-timeout = "command -v niri >/dev/null && niri msg action power-off-monitors || true";
          on-resume = "command -v niri >/dev/null && niri msg action power-on-monitors || true";
        }
        # Suspend after 30 minutes
        {
          timeout = 1800;
          on-timeout = "systemctl suspend";
        }
      ];
    };
  };
}
