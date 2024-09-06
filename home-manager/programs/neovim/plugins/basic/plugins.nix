{ vimUtils, sources }: {
  vimdoc-ja = vimUtils.buildVimPlugin {
    pname = sources.vimdoc-ja.pname;
    version = sources.vimdoc-ja.date;
    src = sources.vimdoc-ja.src;
  };
}
