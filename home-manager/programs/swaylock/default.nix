{ pkgs }:
{
  xdg.configFile = {
    "swaylock/config".source = ./config;
    "swaylock/dracula-wallpaper.svg".source = ./dracula-wallpaper.svg;
  };

  programs.swaylock = {
    enable = pkgs.stdenv.isLinux;
  };
}
