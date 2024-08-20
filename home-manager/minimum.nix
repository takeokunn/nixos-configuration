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

  # services
  services = import ./services;
in {
  imports = basicPrograms ++ services;
  home.stateVersion = "24.05";
  home.packages = basicPkgs;
}
