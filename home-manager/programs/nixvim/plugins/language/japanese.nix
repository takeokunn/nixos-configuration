{ pkgs, customPackages }:
{
  plugins.skkeleton = {
    enable = true;
    eggLikeNewline = true;
    keepState = true;
    sources = [ "skk_server" ];
    kanaTable = "azik";
    azikTable = "us";
    customKanaTable = {
      ss = [ "せい" ];
    };
  };

  extraPlugins = [
    pkgs.vimPlugins.vim-manpager
    customPackages.vimdoc-ja
  ];

  keymaps = [
    {
      mode = [
        "i"
        "c"
      ];
      key = "<C-j>";
      action = "<Plug>(skkeleton-toggle)";
      options.silent = true;
    }
  ];
}
