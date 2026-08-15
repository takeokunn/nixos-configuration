[
  (final: prev: {
    direnv = prev.direnv.overrideAttrs (_: {
      doCheck = false;
    });
  })
]
