{ pkgs }:
{
  services.wlsunset = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;

    # Tokyo coordinates
    latitude = "35.6762";
    longitude = "139.6503";

    # Color temperature
    temperature = {
      day = 6500;
      night = 4000;
    };

    # Gamma (1.0 = no change)
    gamma = "1.0";

    # Systemd service settings
    systemdTarget = "graphical-session.target";
  };
}
