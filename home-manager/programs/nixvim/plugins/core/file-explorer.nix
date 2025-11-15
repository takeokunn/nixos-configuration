{ pkgs, customPackages }:
{
  extraPlugins = [
    customPackages.vim-fern
    customPackages.vim-nerdfont
    customPackages.vim-fern-renderer-nerdfont
  ];

  # extraConfigLua = ''
  #   vim.g["fern#drawer_width"] = 50
  #   vim.keymap.set('n', '<leader>e', '<cmd>Fern . -right -drawer<cr>')
  #   vim.g["fern#renderer"] = "nerdfont"
  # '';
}
