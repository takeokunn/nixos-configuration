{ pkgs, ... }:
{
  # SwayOSD libinput backend - requires root for /dev/input access
  # Reference: https://wiki.nixos.org/wiki/Swayosd
  systemd.services.swayosd-libinput-backend.description = "SwayOSD LibInput Backend";
  systemd.services.swayosd-libinput-backend.wantedBy = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.partOf = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.after = [ "graphical.target" ];
  systemd.services.swayosd-libinput-backend.serviceConfig = {
    Type = "dbus";
    BusName = "org.erikreider.swayosd";
    ExecStart = "${pkgs.swayosd}/bin/swayosd-libinput-backend";
    Restart = "on-failure";
    # It only reads input devices and talks D-Bus.
    NoNewPrivileges = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateNetwork = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    RestrictNamespaces = true;
    LockPersonality = true;
    MemoryDenyWriteExecute = true;
    SystemCallArchitectures = "native";
  };

  # D-Bus policy for swayosd (allows root to own org.erikreider.swayosd bus name)
  services.dbus.packages = [ pkgs.swayosd ];

  services.udev.packages = [ pkgs.swayosd ];
}
