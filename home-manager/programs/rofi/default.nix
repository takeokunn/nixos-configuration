{ pkgs }:
{
  home.file.".config/rofi/themes/dracula.rasi".source = ./themes/dracula.rasi;
  programs.rofi = {
    enable = pkgs.stdenv.isLinux;
    font = "HackGenNerd 16";
    plugins = with pkgs; [
      rofi-calc
      rofi-power-menu
    ];

    pass = {
      enable = pkgs.stdenv.isLinux;
      stores = [ "$HOME/ghq/github.com/takeokunn/private/password-store" ];
    };

    extraConfig = {
      modi = "window,run,drun";
      show-icons = true;
      icon-theme = "Nordzy-dark";
      sidebar-mode = true;
      display-window = " Window";
      display-run = "  Run";
      display-drun = " Application";
      display-filebrowser = " Filebrowser";
      # timeout = {
      #   action = "kb-cancel";
      #   delay = 60;
      # };
      # filebrowser = {
      #   directory = "$HOME";
      #   directories-first = true;
      #   sorting-method = "name";
      # };
    };
    theme = "dracula";
  };
}
