let
  lock = "pidof hyprlock || hyprlock";
  monitors = state: "command -v niri >/dev/null && niri msg action power-${state}-monitors || true";
in
{
  services.hypridle.enable = true;
  services.hypridle.settings = {
    general = {
      lock_cmd = lock;
      before_sleep_cmd = "loginctl lock-session";
      after_sleep_cmd = monitors "on";
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
        on-timeout = lock;
      }
      {
        timeout = 900;
        on-timeout = monitors "off";
        on-resume = monitors "on";
      }
      {
        timeout = 1800;
        on-timeout = "systemctl suspend-then-hibernate";
      }
    ];
  };
}
