{ pkgs }:
{
  plugins.mini = {
    enable = true;
    modules.icons = { };
    mockDevIcons = true;
  };
  plugins.web-devicons.enable = true;
}
