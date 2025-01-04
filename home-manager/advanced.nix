{
  system,
  nixpkgs,
  org-babel,
  emacs-overlay,
}:
let
  # nvfetcher
  sources = pkgs.callPackage ../_sources/generated.nix { };

  # packages
  basicOverlay = import ./overlay/basic.nix;
  advancedOverlay = import ./overlay/advanced.nix { inherit emacs-overlay; };
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay ++ advancedOverlay;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  advancedPkgs = import ./packages/advanced.nix { inherit pkgs; };
  emacsPkg = import ./packages/emacs { inherit pkgs sources; };

  # misc
  misc = import ./misc;

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix { inherit pkgs sources; };
  advancedPrograms = import ./programs/advanced.nix {
    inherit (nixpkgs) lib;
    inherit
      pkgs
      org-babel
      sources
      emacsPkg
      ;
  };

  # services
  basicServices = import ./services/basic.nix;
  advancedServices = import ./services/advanced.nix {
    inherit pkgs emacsPkg;
  };
in
{
  imports = misc ++ modules ++ basicPrograms ++ advancedPrograms ++ basicServices ++ advancedServices;

  home.stateVersion = "24.11";
  home.packages = basicPkgs ++ advancedPkgs;
}
