{ sources, epkgs }: {
  explain-pause-mode = epkgs.melpaBuild {
    pname = "explain-pause-mode";
    version = "0.0.1";
    src = sources.emacs-explain-pause-mode.src;

    packageRequires = with epkgs; [ csv-mode ];

    ignoreCompilationError = false;
  };
}
