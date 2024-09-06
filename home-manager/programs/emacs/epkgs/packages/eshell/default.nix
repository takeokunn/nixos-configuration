{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  esh-help
  eshell-did-you-mean
  eshell-z
  fish-completion
  plugins.eshell-multiple
  plugins.eshell-syntax-highlighting
  plugins.eshell-fringe-status
  # eat
  mistty
]
