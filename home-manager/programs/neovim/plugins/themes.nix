{ pkgs }:
with pkgs.vimPlugins; [
  {
    type = "lua";
    plugin = dracula-nvim;
    config = ''
      vim.cmd[[colorscheme dracula]]
    '';
  }
  {
    type = "lua";
    plugin = lualine-nvim;
    config = ''
      require('lualine').setup {
        options = {
          icons_enabled = false,
          section_separators = "",
          component_separators = ""
        }
      }
    '';
  }
]
