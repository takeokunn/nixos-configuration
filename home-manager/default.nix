{ system, nixpkgs, emacs-overlay }:
let
  lib = nixpkgs.lib;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay.nix { inherit emacs-overlay; };
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  utilsPkgs = import ./packages/utils.nix { inherit pkgs; };
  darwinPkgs = import ./packages/darwin { inherit pkgs; };
  nixosPkgs = import ./packages/nixos { inherit pkgs; };
in {
  home.stateVersion = "23.11";
  home.packages = basicPkgs ++ utilsPkgs
    ++ lib.optionals pkgs.stdenv.isDarwin darwinPkgs
    ++ lib.optionals pkgs.stdenv.isLinux nixosPkgs;
}
