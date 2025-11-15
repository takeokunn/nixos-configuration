{ pkgs, customPlugins }:
{
  extraPlugins = [
    customPlugins.nvim-aibo
    pkgs.vimPlugins.vim-markdown
    pkgs.vimPlugins.goyo-vim
  ];
}
