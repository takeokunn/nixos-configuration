{ epkgs, pkgs, sources }:
let packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in with epkgs; [
  esh-help
  eshell-did-you-mean
  eshell-z
  fish-completion
  packages.eshell-multiple
  packages.eshell-syntax-highlighting
  packages.eshell-fringe-status
  # eat
  mistty
]
