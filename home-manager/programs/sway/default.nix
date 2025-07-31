{
  home.file = {
    ".config/sway/wallpaper.png".source = ./wallpaper.png;
    ".config/sway/config".source = ./config;
    ".config/sway/ime.sh" = {
      source = ./ime.sh;
      executable = true;
    };
    ".config/sway/emacs-ime.sh" = {
      source = ./emacs-ime.sh;
      executable = true;
    };
    ".config/sway/config.d/" = {
      source = ./config.d;
      recursive = true;
    };
  };
}
