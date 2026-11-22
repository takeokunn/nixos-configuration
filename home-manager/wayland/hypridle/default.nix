{ pkgs, ... }:
let
  lock = "pidof hyprlock || hyprlock";
  monitors = state: "command -v niri >/dev/null && niri msg action power-${state}-monitors || true";

  # hypridle has no power-source condition, so each listener asks when its timeout fires:
  # `idle-if ac|battery CMD...` runs CMD only on that power source. Mains and USB-C count as AC.
  idleIf = pkgs.writeShellScript "idle-if" ''
    want=$1
    shift
    source=battery
    for supply in /sys/class/power_supply/*; do
      case "$(cat "$supply/type" 2>/dev/null)" in
        Mains | USB) [ "$(cat "$supply/online" 2>/dev/null)" = 1 ] && source=ac ;;
      esac
    done
    [ "$source" = "$want" ] || exit 0
    exec "$@"
  '';
  # hypridle runs on-resume for every listener whose timeout passed, even when idle-if skipped
  # the dim, so the marker keeps undim from restoring an older saved brightness.
  dim = pkgs.writeShellScript "idle-dim" ''
    brightnessctl -s set 30% && touch "$XDG_RUNTIME_DIR/hypridle-dimmed"
  '';
  undim = pkgs.writeShellScript "idle-undim" ''
    rm "$XDG_RUNTIME_DIR/hypridle-dimmed" 2>/dev/null || exit 0
    brightnessctl -r
  '';
  sh = cmd: "sh -c '${cmd}'";
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

    # On battery: dim at 10 min, lock at 20, screen off at 30, suspend at 60.
    # On AC the screen stays on for an hour: dim at 55 min, lock and screen off at 60, suspend at 120.
    listener = [
      {
        timeout = 600;
        on-timeout = "${idleIf} battery ${dim}";
        on-resume = "${undim}";
      }
      {
        timeout = 1200;
        on-timeout = "${idleIf} battery ${sh lock}";
      }
      {
        timeout = 1800;
        on-timeout = "${idleIf} battery ${sh (monitors "off")}";
        on-resume = monitors "on";
      }
      {
        timeout = 3600;
        on-timeout = "${idleIf} battery systemctl suspend-then-hibernate";
      }
      {
        timeout = 3300;
        on-timeout = "${idleIf} ac ${dim}";
        on-resume = "${undim}";
      }
      {
        timeout = 3600;
        on-timeout = "${idleIf} ac ${sh lock}";
      }
      {
        timeout = 3600;
        on-timeout = "${idleIf} ac ${sh (monitors "off")}";
        on-resume = monitors "on";
      }
      {
        timeout = 7200;
        on-timeout = "${idleIf} ac systemctl suspend-then-hibernate";
      }
    ];
  };
}
