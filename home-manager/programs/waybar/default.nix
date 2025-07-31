{ pkgs }:
{
  xdg.configFile = {
    "waybar/colors.css".source = ./colors.css;
    "waybar/style.css".source = ./style.css;
    "waybar/config".source = ./config;
    "waybar/wittr.sh".source = ./wittr.sh;
  };

  programs.waybar.enable = pkgs.stdenv.isLinux;
}
