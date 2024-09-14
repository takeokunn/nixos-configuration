{ sources, epkgs }: {
  rainbow-csv = epkgs.melpaBuild {
    pname = "rainbow-csv";
    version = "0.0.1";
    src = sources.emacs-rainbow-csv.src;

    packageRequires = with epkgs; [ csv-mode ];

    ignoreCompilationError = false;
  };

  emacs-php-doc-block = epkgs.melpaBuild {
    pname = "php-doc-block";
    version = "0.0.1";
    src = sources.emacs-php-doc-block.src;

    ignoreCompilationError = false;
  };

  fish-repl = epkgs.melpaBuild {
    pname = "fish-repl";
    version = "0.0.1";
    src = sources.emacs-fish-repl.src;

    packageRequires = with epkgs; [ f ];

    ignoreCompilationError = false;
  };
}
