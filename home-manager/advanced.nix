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
  basicOverlay = import ./overlay/basic.nix { inherit emacs-overlay; };
  advancedOverlay = import ./overlay/advanced.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay ++ advancedOverlay;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };
  advancedPkgs = import ./packages/advanced.nix { inherit pkgs; };

  # emacs package
  emacs = import ./packages/emacs { inherit pkgs sources; };
  emacsPkg = emacs.emacs-unstable;

  # misc
  misc = import ./misc;

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix {
    inherit pkgs sources;
    inherit org-babel emacsPkg;
  };
  advancedPrograms = import ./programs/advanced.nix {
    inherit (nixpkgs) lib;
    inherit pkgs sources;
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
