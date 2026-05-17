{ pkgs }:
{
  extraPlugins = [
    pkgs.vimPlugins.vim-gnupg
    pkgs.vimPlugins.vim-suda
  ];
}
