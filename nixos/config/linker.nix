{ pkgs, ... }:
{
  nixpkgs.overlays = [
    (final: prev: {
      stdenv = prev.stdenvAdapters.useMoldLinker prev.stdenv;
    })
  ];

  environment.systemPackages = [ pkgs.mold-wrapped ];
}
