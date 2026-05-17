{ pkgs }:
{
  # SwayOSD libinput backend - requires root for /dev/input access
  # Reference: https://wiki.nixos.org/wiki/Swayosd
  systemd.services.swayosd-libinput-backend.description = "SwayOSD LibInput Backend";
  systemd.services.swayosd-libinput-backend.wantedBy = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.partOf = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.after = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.serviceConfig.Type = "dbus";
  systemd.services.swayosd-libinput-backend.serviceConfig.BusName = "org.erikreider.swayosd";
  systemd.services.swayosd-libinput-backend.serviceConfig.ExecStart =
    "${pkgs.swayosd}/bin/swayosd-libinput-backend";
  systemd.services.swayosd-libinput-backend.serviceConfig.Restart = "on-failure";

  # D-Bus policy for swayosd (allows root to own org.erikreider.swayosd bus name)
  services.dbus.packages = [ pkgs.swayosd ];

  # Ensure udev rules for input device access
  services.udev.packages = [ pkgs.swayosd ];
}
