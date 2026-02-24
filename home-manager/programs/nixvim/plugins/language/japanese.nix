{ pkgs, customPackages }:
{
  extraPlugins = [
    # Japanese documentation
    pkgs.vimPlugins.vim-manpager
    customPackages.vimdoc-ja
  ];
}
