{ sources, epkgs }: {
  copilot = epkgs.melpaBuild {
    pname = "copilot";
    version = "0.0.1";
    src = sources.emacs-copilot.src;

    packageRequires = with epkgs; [ f editorconfig ];

    ignoreCompilationError = false;
  };

  llm = epkgs.melpaBuild {
    pname = "llm";
    version = "0.0.1";
    src = sources.emacs-llm.src;

    packageRequires = with epkgs; [ plz ];

    ignoreCompilationError = false;
  };
}
