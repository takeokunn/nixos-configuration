{ emacs-overlay }:
[
  (import emacs-overlay)
  # bug: https://github.com/NixOS/nixpkgs/issues/395169
  (final: prev: {
    emacs = prev.emacs.override {
      withNativeCompilation = false;
    };
    emacs-unstable = prev.emacs-unstable.override {
      withNativeCompilation = true;
    };
    emacs-git = prev.emacs-git.override {
      withNativeCompilation = false;
    };
    ollama = prev.ollama.overrideAttrs (oldAttrs: {
      version = "0.11.2";
      src = final.fetchFromGitHub {
        owner = "ollama";
        repo = "ollama";
        tag = "v0.11.2";
        hash = "sha256-NZaaCR6nD6YypelnlocPn/43tpUz0FMziAlPvsdCb44=";
      };
      doCheck = false;
    });
  })
]
