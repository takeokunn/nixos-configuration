{ sources, epkgs }: {
  consult-tramp = epkgs.melpaBuild {
    pname = "consult-tramp";
    version = "0.0.1";
    src = sources.emacs-consult-tramp.src;

    packageRequires = with epkgs; [ consult ];

    ignoreCompilationError = false;
  };
}
