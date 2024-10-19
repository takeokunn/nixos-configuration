{ vimUtils, sources }: {
  vimdoc-ja = vimUtils.buildVimPlugin {
    pname = sources.vimdoc-ja.pname;
    version = sources.vimdoc-ja.date;
    src = sources.vimdoc-ja.src;
  };
  vim-fern = vimUtils.buildVimPlugin {
    pname = sources.vim-fern.pname;
    version = sources.vim-fern.date;
    src = sources.vim-fern.src;
  };
  denops-helloworld = vimUtils.buildVimPlugin {
    pname = sources.denops-helloworld.pname;
    version = sources.denops-helloworld.date;
    src = sources.denops-helloworld.src;
  };
}
