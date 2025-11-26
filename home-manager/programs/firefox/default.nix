{ pkgs }:
{
  programs.firefox = {
    enable = pkgs.stdenv.isLinux;
  };
}
