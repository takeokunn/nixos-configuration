{ emacs-overlay }:
[
  (import emacs-overlay)

  # bug: https://github.com/NixOS/nixpkgs/issues/395169
  (final: prev: {
    emacs = prev.emacs.override {
      withNativeCompilation = false;
    };
    emacs-unstable = prev.emacs-unstable.override {
      withNativeCompilation = false;
    };
    emacs-git = prev.emacs-git.override {
      withNativeCompilation = false;
    };
  })
  (final: prev: {
    python3Packages = prev.python3Packages // {
      sentry-sdk = prev.python3Packages.sentry-sdk.overrideAttrs (old: {
        doCheck = false;
      });
    };
  })
  (final: prev: {
    lnav = prev.lnav.overrideAttrs (old: {
      version = "2025-06-11-HEAD";
      src = prev.fetchFromGitHub {
        owner = "tstack";
        repo = "lnav";
        rev = "4640885e61bc639f6c12a0e3208a210e6978fcd8";
        hash = "sha256-XS3/km2sJwRnWloLKu9X9z07+qBFRfUsaRpZVYjoclI=";
      };
    });
  })
]
