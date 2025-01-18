let
  makeConfigFile = filePath: {
    source = filePath;
    executable = true;
  };
in
{
  xdg.configFile = {
    "sketchybar/sketchybarrc" = makeConfigFile ./sketchybarrc;

    # plugins
    "sketchybar/plugins/date.sh" = makeConfigFile ./plugins/date.sh;
    "sketchybar/plugins/power.sh" = makeConfigFile ./plugins/power.sh;
    "sketchybar/plugins/time.sh" = makeConfigFile ./plugins/time.sh;
    "sketchybar/plugins/window_title.sh" = makeConfigFile ./plugins/window_title.sh;
  };
}
