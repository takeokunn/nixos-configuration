{ pkgs, sources }:
{
  # gitsigns
  plugins.gitsigns = {
    enable = true;
    settings = {
      # デフォルト設定を使用
    };
  };

  # vim-gin (カスタムビルド)
  extraPlugins = [
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-gin.pname;
      version = sources.vim-gin.date;
      src = sources.vim-gin.src;
    })
  ];
}
