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
]
