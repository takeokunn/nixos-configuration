{
  home.file = {
    ".config/sway/wallpaper.png".source = ./wallpaper.png;
    ".config/sway/config".source = ./config;
    ".config/sway/config.d/" = {
      source = ./config.d;
      recursive = true;
    };
  };
}
