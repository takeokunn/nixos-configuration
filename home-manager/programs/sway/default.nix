{
  xdg.configFile = {
    "sway/wallpaper.png".source = ./wallpaper.png;
    "sway/config".source = ./config;
    "sway/ime.sh" = {
      source = ./ime.sh;
      executable = true;
    };
    "sway/emacs-ime.sh" = {
      source = ./emacs-ime.sh;
      executable = true;
    };
    "sway/config.d/" = {
      source = ./config.d;
      recursive = true;
    };
  };
}
