{ epkgs, pkgs, sources }:
let packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in with epkgs; [
  esh-help
  eshell-did-you-mean
  eshell-z
  fish-completion
  packages.eshell-multiple
  eshell-syntax-highlighting
  eshell-fringe-status
  # eat
  mistty
]
