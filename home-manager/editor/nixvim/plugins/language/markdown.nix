{ pkgs, customPackages }:
{
  extraPlugins = [
    customPackages.nvim-aibo
    pkgs.vimPlugins.vim-markdown
    pkgs.vimPlugins.goyo-vim
  ];
}
