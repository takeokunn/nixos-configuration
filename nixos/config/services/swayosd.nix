{ pkgs }:
{
  # SwayOSD libinput backend - requires root for /dev/input access
  # Reference: https://wiki.nixos.org/wiki/Swayosd
  systemd.services.swayosd-libinput-backend = {
    description = "SwayOSD LibInput Backend";
    wantedBy = [ "graphical.target" ];
    partOf = [ "graphical.target" ];
    after = [ "graphical.target" ];

    serviceConfig = {
      Type = "dbus";
      BusName = "org.erikreider.swayosd";
      ExecStart = "${pkgs.swayosd}/bin/swayosd-libinput-backend";
      Restart = "on-failure";
    };
  };

  # D-Bus policy for swayosd (allows root to own org.erikreider.swayosd bus name)
  services.dbus.packages = [ pkgs.swayosd ];

  # Ensure udev rules for input device access
  services.udev.packages = [ pkgs.swayosd ];
}
