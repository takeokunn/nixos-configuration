{ pkgs }:
{
  services.easyeffects = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
  };
}
