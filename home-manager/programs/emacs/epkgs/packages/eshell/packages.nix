{ sources, epkgs }: {
  eshell-fringe-status = epkgs.melpaBuild {
    pname = "eshell-fringe-status";
    version = "0.0.1";
    src = sources.emacs-eshell-fringe-status.src;

    ignoreCompilationError = false;
  };

  eshell-multiple = epkgs.melpaBuild {
    pname = "eshell-multiple";
    version = "0.0.1";
    src = sources.emacs-eshell-multiple.src;

    ignoreCompilationError = false;
  };

  eshell-syntax-highlighting = epkgs.melpaBuild {
    pname = "eshell-syntax-highlighting";
    version = "0.0.1";
    src = sources.emacs-eshell-syntax-highlighting.src;

    ignoreCompilationError = false;
  };
}
