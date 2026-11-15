{
  # Also turns off the TLP that nixos-hardware enables when no other power daemon is set.
  services.power-profiles-daemon.enable = true;

  services.upower = {
    enable = true;
    percentageLow = 10;
    percentageCritical = 7;
    percentageAction = 5;
    criticalPowerAction = "Hibernate";
  };

  # Stop charging at 80% and resume below 75% to slow battery wear.
  systemd.tmpfiles.rules = [
    "w /sys/class/power_supply/BAT0/charge_control_start_threshold - - - - 75"
    "w /sys/class/power_supply/BAT0/charge_control_end_threshold - - - - 80"
  ];

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend-then-hibernate";
    HandleLidSwitchExternalPower = "suspend-then-hibernate";
    HandleLidSwitchDocked = "ignore";
  };
  systemd.sleep.settings.Sleep.HibernateDelaySec = "2h";
}
