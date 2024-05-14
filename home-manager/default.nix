{ system, nixpkgs, emacs-overlay, ... }:
let
  lib = nixpkgs.lib;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay.nix { inherit emacs-overlay; };
  };
  basePkgs = import ./packages/base.nix { inherit pkgs; };
  darwinPkgs = import ./packages/darwin.nix { inherit pkgs; };
  nixosPkgs = import ./packages/nixos.nix { inherit pkgs; };
in {
  home.stateVersion = "23.11";
  home.packages = basePkgs ++ lib.optionals pkgs.stdenv.isDarwin darwinPkgs
    ++ lib.optionals pkgs.stdenv.isLinux nixosPkgs;
}
