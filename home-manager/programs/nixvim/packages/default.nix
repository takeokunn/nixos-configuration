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
