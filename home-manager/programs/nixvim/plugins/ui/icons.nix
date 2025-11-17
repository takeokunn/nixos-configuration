{ pkgs }:
{
  plugins.web-devicons.enable = false;

  plugins.mini = {
    enable = true;
    modules.icons = { };
    mockDevIcons = true;
  };
}
