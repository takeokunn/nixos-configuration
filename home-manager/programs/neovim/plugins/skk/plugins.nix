{ vimUtils, sources }: {

  skkeleton = vimUtils.buildVimPlugin {
    pname = sources.skkeleton.pname;
    version = sources.skkeleton.date;
    src = sources.skkeleton.src;
  };
}
