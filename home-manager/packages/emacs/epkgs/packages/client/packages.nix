{ sources, epkgs }:
{
  mu4e-dashboard = epkgs.melpaBuild {
    pname = "mu4e-dashboard";
    version = "0.0.1";
    src = sources.emacs-mu4e-dashboard.src;

    packageRequires = with epkgs; [
      mu4e
      async
    ];

    ignoreCompilationError = false;
  };
}
