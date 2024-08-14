{ system, nixpkgs }:
let
  # packages
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # programs
  programs = import ./programs { inherit pkgs; };
in {
  imports = programs;
  home.stateVersion = "24.05";
  home.packages = basicPkgs;
}
