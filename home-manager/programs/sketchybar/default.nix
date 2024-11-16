let
  makeConfigFile = filePath: {
    source = filePath;
    executable = true;
  };
in
{
  xdg.configFile = {
    "sketchybar/sketchybarrc" = makeConfigFile ./sketchybarrc;

    # items
    "sketchybar/items/battery.sh" = makeConfigFile ./items/battery.sh;
    "sketchybar/items/calendar.sh" = makeConfigFile ./items/calendar.sh;
    "sketchybar/items/cpu.sh" = makeConfigFile ./items/cpu.sh;
    "sketchybar/items/volume.sh" = makeConfigFile ./items/volume.sh;

    # plugins
    "sketchybar/plugins/aerospace.sh" = makeConfigFile ./plugins/aerospace.sh;
    "sketchybar/plugins/battery.sh" = makeConfigFile ./plugins/battery.sh;
    "sketchybar/plugins/calendar.sh" = makeConfigFile ./plugins/calendar.sh;
    "sketchybar/plugins/clock.sh" = makeConfigFile ./plugins/clock.sh;
    "sketchybar/plugins/cpu.sh" = makeConfigFile ./plugins/cpu.sh;
    "sketchybar/plugins/date.sh" = makeConfigFile ./plugins/date.sh;
    "sketchybar/plugins/net.sh" = makeConfigFile ./plugins/net.sh;
    "sketchybar/plugins/power.sh" = makeConfigFile ./plugins/power.sh;
    "sketchybar/plugins/time.sh" = makeConfigFile ./plugins/time.sh;
    "sketchybar/plugins/volume.sh" = makeConfigFile ./plugins/volume.sh;
    "sketchybar/plugins/window_title.sh" = makeConfigFile ./plugins/window_title.sh;
  };
}
