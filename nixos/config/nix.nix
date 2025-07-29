{
  nix = {
    settings.cores = 8;
    optimise.automatic = true;
  };

  nixpkgs.overlays = [
    (final: prev: {
      stdenv = prev.stdenvAdapters.useMold prev.stdenv;
    })
  ];
}
