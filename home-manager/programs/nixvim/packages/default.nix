{ pkgs, sources }:
{
  skkeleton = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-skkeleton.pname;
    version = sources.vim-skkeleton.date;
    src = sources.vim-skkeleton.src;
  };

  skkeleton-azik = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-skkeleton-azik.pname;
    version = sources.vim-skkeleton-azik.date;
    src = sources.vim-skkeleton-azik.src;
  };

  vimdoc-ja = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vimdoc-ja.pname;
    version = sources.vimdoc-ja.date;
    src = sources.vimdoc-ja.src;
  };

  vim-fern = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-fern.pname;
    version = sources.vim-fern.date;
    src = sources.vim-fern.src;
  };

  vim-nerdfont = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-nerdfont.pname;
    version = sources.vim-nerdfont.date;
    src = sources.vim-nerdfont.src;
  };

  vim-fern-renderer-nerdfont = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-fern-renderer-nerdfont.pname;
    version = sources.vim-fern-renderer-nerdfont.date;
    src = sources.vim-fern-renderer-nerdfont.src;
  };

  vim-gin = pkgs.vimUtils.buildVimPlugin {
    pname = sources.vim-gin.pname;
    version = sources.vim-gin.date;
    src = sources.vim-gin.src;
  };

  nvim-aibo = pkgs.vimUtils.buildVimPlugin {
    pname = sources.nvim-aibo.pname;
    version = sources.nvim-aibo.version;
    src = sources.nvim-aibo.src;
    dontInstallDoc = true;
    doCheck = false;
    nativeBuildInputs = [ ];
    buildInputs = [ ];
    fixupPhase = ''
      echo "Skipping help tag generation for nvim-aibo"
    '';
  };
}
