{ system, nixpkgs, emacs-overlay }:
let
  lib = nixpkgs.lib;

  # packages
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay { inherit emacs-overlay; };
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  advancedPkgs = import ./packages/advanced.nix { inherit pkgs; };
  darwinPkgs = import ./packages/darwin { inherit pkgs; };
  nixosPkgs = import ./packages/nixos { inherit pkgs; };

  # programs
  programs = import ./programs;
in {
  imports = programs;

  home.stateVersion = "24.11";
  home.packages = basicPkgs ++ advancedPkgs
    ++ lib.optionals pkgs.stdenv.isDarwin darwinPkgs
    ++ lib.optionals pkgs.stdenv.isLinux nixosPkgs;
}
