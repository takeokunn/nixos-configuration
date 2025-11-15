{ pkgs }:
{
  plugins.mini = {
    enable = true;
    modules.icons = { };
    mockDevIcons = true;
  };
}
