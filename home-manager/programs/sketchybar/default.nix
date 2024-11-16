{
  xdg.configFile = {
    "sketchybar/sketchybarrc" = {
      source = ./sketchybarrc;
      executable = true;
    };
    "sketchybar/items" = {
      source = ./items;
      executable = true;
      recursive = true;
    };
    "sketchybar/plugins" = {
      source = ./plugins;
      executable = true;
      recursive = true;
    };
    "sketchybar/scripts" = {
      source = ./scripts;
      executable = true;
      recursive = true;
    };
  };
}
