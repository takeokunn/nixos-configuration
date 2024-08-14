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

  # programs
  programs = import ./programs { inherit pkgs; };
in {
  imports = programs;
  home.stateVersion = "24.11";
  home.packages = basicPkgs;
}
