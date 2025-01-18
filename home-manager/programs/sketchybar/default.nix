{
  xdg.configFile = {
    "sketchybar/sketchybarrc" = {
      source = ./sketchybarrc;
      executable = true;
    };

    # plugins
    "sketchybar/plugins/" = {
      source = ./plugins;
      recursive = true;
    };
  };
}
