{
  xdg.configFile = {
    "sway/wallpaper.png".source = ./wallpaper.png;
    "sway/config".source = ./config;
    "sway/config.d/" = {
      source = ./config.d;
      recursive = true;
    };
  };
}
