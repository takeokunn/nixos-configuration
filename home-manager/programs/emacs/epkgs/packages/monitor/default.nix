{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  proced-narrow
  symon
  esup
  plugins.explain-pause-mode
  disk-usage
  keyfreq
  uptimes
]
