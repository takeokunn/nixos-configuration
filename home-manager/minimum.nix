{ system, nixpkgs }:
let
  # packages
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix { inherit pkgs; };

  # services
  basicServices = import ./services/basic.nix;
in {
  imports = modules ++ basicPrograms ++ basicServices;
  home.stateVersion = "24.05";
  home.packages = basicPkgs;
}
