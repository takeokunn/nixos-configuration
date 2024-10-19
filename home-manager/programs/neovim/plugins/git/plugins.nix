{ vimUtils, sources }: {

  vim-gin = vimUtils.buildVimPlugin {
    pname = sources.vim-gin.pname;
    version = sources.vim-gin.date;
    src = sources.vim-gin.src;
  };
}
