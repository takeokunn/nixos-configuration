[
  (final: prev: {
    # direnv test-zsh deadlocks on macOS due to /etc/zshenv inflating the env past the 64KB pipe buffer
    direnv = prev.direnv.overrideAttrs (_: {
      doCheck = false;
    });

    # tmux 3.7c's configure now requires an explicit jemalloc choice on Darwin
    # (macOS calloc doesn't reliably zero); nixpkgs' package.nix doesn't pass
    # either flag yet. jemalloc isn't a buildInput here, so disable it.
    tmux = prev.tmux.overrideAttrs (old: {
      configureFlags = old.configureFlags ++ prev.lib.optionals prev.stdenv.hostPlatform.isDarwin [
        "--disable-jemalloc"
      ];
    });
  })
]
