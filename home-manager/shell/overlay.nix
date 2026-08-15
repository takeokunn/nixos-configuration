[
  (final: prev: {
    # direnv test-zsh deadlocks on macOS due to /etc/zshenv inflating the env past the 64KB pipe buffer
    direnv = prev.direnv.overrideAttrs (_: {
      doCheck = false;
    });
  })
]
