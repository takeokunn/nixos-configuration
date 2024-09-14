{ sources, epkgs }: {
  pinentry-el = epkgs.melpaBuild {
    pname = "pinentry";
    version = "0.0.1";
    src = sources.emacs-pinentry.src;

    ignoreCompilationError = false;
  };

  sudden-death = epkgs.melpaBuild {
    pname = "sudden-death";
    version = "0.0.1";
    src = sources.emacs-sudden-death.src;

    ignoreCompilationError = false;
  };
}
