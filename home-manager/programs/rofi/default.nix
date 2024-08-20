{ lib, pkgs }: {
  programs.rofi = {
    enable = pkgs.stdenv.isLinux;
    font = "Cica 16";
    plugins = with pkgs; [ rofi-calc rofi-power-menu ];

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
    theme = builtins.readFile ./themes/dracula.rasi;
  };
}
