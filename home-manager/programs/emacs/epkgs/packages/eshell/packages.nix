{ sources, epkgs }: {
  eshell-multiple = epkgs.melpaBuild {
    pname = "eshell-multiple";
    version = "0.0.1";
    src = sources.emacs-eshell-multiple.src;

    ignoreCompilationError = false;
  };
}
