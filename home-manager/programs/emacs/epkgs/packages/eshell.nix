{ epkgs, pkgs }:
let
  eshell-fringe-status =
    pkgs.callPackage ./nixpkgs/eshell-fringe-status.nix { inherit epkgs; };
  eshell-multiple =
    pkgs.callPackage ./nixpkgs/eshell-multiple.nix { inherit epkgs; };
  eshell-syntax-highlighting =
    pkgs.callPackage ./nixpkgs/eshell-syntax-highlighting.nix {
      inherit epkgs;
    };
in with epkgs; [
  eshell-fringe-status
  esh-help
  eshell-multiple
  eshell-z
  eshell-did-you-mean
  eshell-syntax-highlighting
  fish-completion
  # eat
  mistty
]
