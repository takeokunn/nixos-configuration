{ pkgs }:
{
  xdg.configFile = {
    "rofi/themes/dracula.rasi".source = ./themes/dracula.rasi;
  };

  programs.rofi = {
    enable = pkgs.stdenv.isLinux;
    theme = "dracula";
    plugins = with pkgs; [
      rofi-calc
      rofi-power-menu
    ];

    extraConfig = {
      show-icons = true;
      icon-theme = "Nordzy-dark";
      sidebar-mode = true;
      display-window = " Window";
      display-run = "  Run";
      display-drun = " Application";
      display-filebrowser = " Filebrowser";
      disable-history = false;
    };
  };
}
