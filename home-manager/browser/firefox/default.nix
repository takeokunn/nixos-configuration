{ pkgs }:
{
  programs.firefox = {
    enable = pkgs.stdenv.isLinux;
    configPath = ".mozilla/firefox";
  };
}
