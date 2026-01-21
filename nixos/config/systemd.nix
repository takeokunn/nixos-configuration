{
  systemd.services = {
    "getty@tty1".enable = false;
    "autovt@tty1".enable = false;
    nix-daemon.serviceConfig = {
      MemoryMax = "12G";
      MemoryHigh = "10G";
      OOMScoreAdjust = 500;
    };
  };
}
