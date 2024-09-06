{ sources, epkgs }: {
  consult-tramp = epkgs.melpaBuild {
    pname = "consult-tramp";
    version = "0.0.1";
    src = sources.emacs-consult-tramp.src;

    ignoreCompilationError = false;
  };
}
