{ pkgs }:
{
  home.file = {
    ".config/waybar/style.css".source = ./style.css;
    ".config/waybar/config".source = ./config;
  };

  programs.waybar.enable = pkgs.stdenv.isLinux;
}
