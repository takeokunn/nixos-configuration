{ system, nixpkgs, emacs-overlay }:
let
  # packages
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay { inherit emacs-overlay; };
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # programs
  # programs = import ./programs/minimum.nix { inherit pkgs; };
in {
  # imports = programs;
  home.stateVersion = "24.05";
  home.packages = basicPkgs;
}
