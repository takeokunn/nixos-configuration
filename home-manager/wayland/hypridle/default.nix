{
  services.hypridle.enable = true;
  services.hypridle.settings = {
    general = {
      lock_cmd = "pidof hyprlock || hyprlock";
      before_sleep_cmd = "loginctl lock-session";
      after_sleep_cmd = "command -v niri >/dev/null && niri msg action power-on-monitors || true";
      ignore_dbus_inhibit = false;
      ignore_systemd_inhibit = false;
    };

    listener = [
      {
        timeout = 300;
        on-timeout = "brightnessctl -s set 30%";
        on-resume = "brightnessctl -r";
      }
      {
        timeout = 600;
        on-timeout = "pidof hyprlock || hyprlock";
      }
      {
        timeout = 900;
        on-timeout = "command -v niri >/dev/null && niri msg action power-off-monitors || true";
        on-resume = "command -v niri >/dev/null && niri msg action power-on-monitors || true";
      }
      {
        timeout = 1800;
        on-timeout = "systemctl suspend";
      }
    ];
  };
}
