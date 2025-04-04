{ sources, epkgs }:
{
  org-view-mode = epkgs.melpaBuild {
    pname = "org-view-mode";
    version = "0.0.1";
    src = sources.emacs-org-view-mode.src;

    ignoreCompilationError = false;
  };

  org-dashboard = epkgs.melpaBuild {
    pname = "org-dashboard";
    version = "0.0.1";
    src = sources.emacs-org-dashboard.src;

    ignoreCompilationError = false;
  };

  org-volume = epkgs.melpaBuild {
    pname = "org-volume";
    version = "0.0.1";
    src = sources.emacs-org-volume.src;

    packageRequires = with epkgs; [
      request
      dash
      f
    ];

    ignoreCompilationError = false;
  };

  ob-phpstan = epkgs.melpaBuild {
    pname = "ob-phpstan";
    version = "0.0.1";
    src = sources.emacs-ob-phpstan.src;

    ignoreCompilationError = false;
  };

  ob-racket = epkgs.melpaBuild {
    pname = "ob-racket";
    version = "0.0.1";
    src = sources.emacs-ob-racket.src;

    ignoreCompilationError = false;
  };

  ob-treesitter = epkgs.melpaBuild {
    pname = "ob-treesitter";
    version = "0.0.1";
    src = sources.emacs-ob-treesitter.src;

    ignoreCompilationError = false;
  };

  ob-fish = epkgs.melpaBuild {
    pname = "ob-fish";
    version = "0.0.1";
    src = sources.emacs-ob-fish.src;

    ignoreCompilationError = false;
  };

  ox-hatena = epkgs.melpaBuild {
    pname = "ox-hatena";
    version = "0.0.1";
    src = sources.emacs-ox-hatena.src;

    ignoreCompilationError = false;
  };

  ox-typst = epkgs.melpaBuild {
    pname = "ox-typst";
    version = "0.0.1";
    src = sources.emacs-ox-typst.src;

    ignoreCompilationError = false;
  };
}
