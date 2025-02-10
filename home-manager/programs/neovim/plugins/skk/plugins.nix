{ vimUtils, sources }:
{

  skkeleton = vimUtils.buildVimPlugin {
    pname = sources.vim-skkeleton.pname;
    version = sources.vim-skkeleton.date;
    src = sources.vim-skkeleton.src;
  };

  skkeleton-azik = vimUtils.buildVimPlugin {
    pname = sources.vim-skkeleton-azik.pname;
    version = sources.vim-skkeleton-azik.date;
    src = sources.vim-skkeleton-azik.src;
  };
}
