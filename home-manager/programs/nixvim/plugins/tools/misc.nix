{ pkgs }:
{
  extraPlugins = [
    pkgs.vimPlugins.editorconfig-nvim
    pkgs.vimPlugins.vim-gnupg
    pkgs.vimPlugins.vim-suda
  ];
}
