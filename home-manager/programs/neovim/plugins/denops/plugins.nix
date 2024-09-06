{ vimUtils, sources }: {
  denops-helloworld = vimUtils.buildVimPlugin {
    pname = sources.denops-helloworld.pname;
    version = sources.denops-helloworld.date;
    src = sources.denops-helloworld.src;
  };

  skkeleton = vimUtils.buildVimPlugin {
    pname = sources.skkeleton.pname;
    version = sources.skkeleton.date;
    src = sources.skkeleton.src;
  };
}
