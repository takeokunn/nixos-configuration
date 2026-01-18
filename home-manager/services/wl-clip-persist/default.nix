{ pkgs }:
{
  systemd.user.services.wl-clip-persist = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "Persist clipboard contents after application close";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.wl-clip-persist}/bin/wl-clip-persist --clipboard both";
      Restart = "on-failure";
      RestartSec = 1;
    };
  };
}
