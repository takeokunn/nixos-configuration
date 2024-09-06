{ sources, epkgs }: {
  bazel-mode = epkgs.melpaBuild {
    pname = "bazel";
    version = "0.0.1";
    src = sources.emacs-bazel-mode.src;

    ignoreCompilationError = false;
  };

  direnv-mode = epkgs.melpaBuild {
    pname = "direnv";
    version = "0.0.1";
    src = sources.emacs-direnv-mode.src;

    packageRequires = with epkgs; [ dash ];

    ignoreCompilationError = false;
  };

  systemd-mode = epkgs.melpaBuild {
    pname = "systemd";
    version = "0.0.1";
    src = sources.emacs-systemd-mode.src;
    files = ''("*.el" "*.txt")'';

    ignoreCompilationError = false;
  };

  web-php-blade-mode = epkgs.melpaBuild {
    pname = "web-php-blade-mode";
    version = "0.0.1";
    src = sources.emacs-web-php-blade-mode.src;

    ignoreCompilationError = false;
  };
}
