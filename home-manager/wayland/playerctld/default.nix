{ pkgs }:
{
  services.playerctld = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
  };
}
