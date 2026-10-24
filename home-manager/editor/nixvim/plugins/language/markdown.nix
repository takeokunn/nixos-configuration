{ pkgs, ... }:
{
  extraPlugins = [
    pkgs.vimPlugins.vim-markdown
    pkgs.vimPlugins.goyo-vim
  ];
}
