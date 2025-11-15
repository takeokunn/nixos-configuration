{ pkgs }:
{
  plugins.lualine = {
    enable = true;
    settings = {
      options = {
        icons_enabled = false;
        section_separators = "";
        component_separators = "";
      };
    };
  };
}
