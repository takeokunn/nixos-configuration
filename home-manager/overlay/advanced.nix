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
]
