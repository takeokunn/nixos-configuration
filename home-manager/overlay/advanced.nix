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
      version = "2025-05-03-HEAD";
      src = prev.fetchFromGitHub {
        owner = "tstack";
        repo = "lnav";
        rev = "adec0dc8ec0ffd5a7f06c19852edc35d00dc4fe1";
        hash = "sha256-Dk+pFg+Tt1krFmEzoT4sW8dPd0x5kCD6vTIQVzAvs3A=";
      };
    });
  })
]
