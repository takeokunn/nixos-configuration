{
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;
  systemd.services.nix-daemon.serviceConfig.MemoryMax = "12G";
  systemd.services.nix-daemon.serviceConfig.MemoryHigh = "10G";
  systemd.services.nix-daemon.serviceConfig.OOMScoreAdjust = 500;
}
