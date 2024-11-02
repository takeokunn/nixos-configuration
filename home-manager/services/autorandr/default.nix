{ pkgs }:
{
  services.autorandr = {
    enable = pkgs.stdenv.isLinux;
    ignoreLid = false;
  };
}
