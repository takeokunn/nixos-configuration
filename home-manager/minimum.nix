{ system, nixpkgs }:
let
  # packages
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # programs
  basicPrograms = import ./programs/basic.nix { inherit pkgs; };
in {
  imports = basicPrograms;
  home.stateVersion = "24.05";
  home.packages = basicPkgs;
}
