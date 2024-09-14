{ epkgs, pkgs, sources }:
let packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in with epkgs; [
  proced-narrow
  symon
  esup
  packages.explain-pause-mode
  disk-usage
  keyfreq
  uptimes
]
