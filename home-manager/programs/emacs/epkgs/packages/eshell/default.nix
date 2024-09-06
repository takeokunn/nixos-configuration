{ epkgs, pkgs, sources }:
let
  language_specific = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  esh-help
  eshell-did-you-mean
  eshell-z
  fish-completion
  language_specific.eshell-multiple
  language_specific.eshell-syntax-highlighting
  language_specific.eshell-fringe-status
  # eat
  mistty
]
