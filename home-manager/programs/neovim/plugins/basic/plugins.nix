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
}
