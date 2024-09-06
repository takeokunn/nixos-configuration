{ sources, epkgs }: {
  flycheck-projectile = epkgs.melpaBuild {
    pname = "flycheck-projectile";
    version = "0.0.1";
    src = sources.emacs-flycheck-projectile.src;

    packageRequires = with epkgs; [ flycheck ];

    ignoreCompilationError = false;
  };
}
