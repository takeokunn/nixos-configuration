{
  home.file = {
    ".config/sway/config".source = ./config;
    ".config/sway/config.d/" = {
      source = ./config.d;
      recursive = true;
    };
  };
}
