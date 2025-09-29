{ vimUtils, sources }:
{
  vimdoc-ja = vimUtils.buildVimPlugin {
    pname = sources.vimdoc-ja.pname;
    version = sources.vimdoc-ja.date;
    src = sources.vimdoc-ja.src;
  };
  vim-nerdfont = vimUtils.buildVimPlugin {
    pname = sources.vim-nerdfont.pname;
    version = sources.vim-nerdfont.date;
    src = sources.vim-nerdfont.src;
  };
  vim-fern = vimUtils.buildVimPlugin {
    pname = sources.vim-fern.pname;
    version = sources.vim-fern.date;
    src = sources.vim-fern.src;
  };
  vim-fern-renderer-nerdfont = vimUtils.buildVimPlugin {
    pname = sources.vim-fern-renderer-nerdfont.pname;
    version = sources.vim-fern-renderer-nerdfont.date;
    src = sources.vim-fern-renderer-nerdfont.src;
  };
  nvim-aibo = vimUtils.buildVimPlugin {
    pname = sources.nvim-aibo.pname;
    version = sources.nvim-aibo.version;
    src = sources.nvim-aibo.src;
    # ヘルプタグ生成エラーを回避
    dontInstallDoc = true;
    doCheck = false;
    # vimPluginGenTagsフックを完全に無効化
    nativeBuildInputs = [];
    buildInputs = [];
    # fixupPhaseを完全にオーバーライド
    fixupPhase = ''
      # ヘルプタグ生成をスキップし、何もしない
      echo "Skipping help tag generation for nvim-aibo"
    '';
  };
}
