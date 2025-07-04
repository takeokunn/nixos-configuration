{ sources, epkgs }:
{
  aio = epkgs.melpaBuild {
    pname = "aio";
    version = "0.0.1";
    src = sources.emacs-aio.src;

    ignoreCompilationError = false;
  };

  copilot = epkgs.melpaBuild {
    pname = "copilot";
    version = "0.0.1";
    src = sources.emacs-copilot.src;

    packageRequires = with epkgs; [
      f
      editorconfig
    ];

    ignoreCompilationError = false;
  };

  plz-media-type = epkgs.melpaBuild {
    pname = "plz-media-type";
    version = "0.0.1";
    src = sources.emacs-plz-media-type.src;

    packageRequires = with epkgs; [ plz ];

    ignoreCompilationError = false;
  };

  plz-event-source = epkgs.melpaBuild {
    pname = "plz-event-source";
    version = "0.0.1";
    src = sources.emacs-plz-event-source.src;

    packageRequires = with epkgs; [
      plz
      plz-media-type
    ];

    ignoreCompilationError = false;
  };

  llm = epkgs.melpaBuild {
    pname = "llm";
    version = "0.0.1";
    src = sources.emacs-llm.src;

    packageRequires = with epkgs; [
      plz
      plz-media-type
      plz-event-source
    ];

    ignoreCompilationError = false;
  };
}
