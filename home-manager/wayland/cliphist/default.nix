{ pkgs }:
{
  services.cliphist = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    allowImages = true;
    systemdTargets = [ "graphical-session.target" ];
  };
}
