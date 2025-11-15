{ pkgs, customPackages }:
{
  plugins.gitsigns = {
    enable = true;
    settings = {
    };
  };

  extraPlugins = [
    customPackages.vim-gin
  ];
}
