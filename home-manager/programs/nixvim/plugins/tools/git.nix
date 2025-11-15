{ pkgs, customPlugins }:
{
  plugins.gitsigns = {
    enable = true;
    settings = {
    };
  };

  extraPlugins = [
    customPlugins.vim-gin
  ];
}
