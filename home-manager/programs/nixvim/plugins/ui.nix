{ pkgs }:
{
  # dracula colorscheme
  colorschemes.dracula.enable = true;

  # web-devicons (telescope, lspsagaで自動的に有効化される警告を抑制)
  plugins.web-devicons.enable = true;

  # lualine
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

  # dashboard
  plugins.dashboard = {
    enable = true;
  };
}
