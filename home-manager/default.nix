{ system, nixpkgs, emacs-overlay, wezterm-flake }:
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
  darwinPkgs = import ./packages/darwin.nix { inherit pkgs; };

  # programs
  basicPrograms = import ./programs/basic.nix { inherit pkgs; };
  advancedPrograms =
    import ./programs/advanced.nix { inherit lib pkgs wezterm-flake; };

  # services
  basicServices = import ./services/basic.nix;
  advancedServices = import ./services/advanced.nix { inherit pkgs; };
in {
  imports = basicPrograms ++ advancedPrograms ++ basicServices
    ++ advancedServices;

  home.stateVersion = "24.05";
  home.packages = basicPkgs ++ advancedPkgs
    ++ lib.optionals pkgs.stdenv.isDarwin darwinPkgs;
}
