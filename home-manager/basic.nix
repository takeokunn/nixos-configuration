{
  system,
  nixpkgs,
  org-babel,
}:
let
  # nvfetcher
  sources = pkgs.callPackage ../_sources/generated.nix { };

  # packages
  basicOverlay = import ./overlay/basic.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # emacs package
  emacs = import ./packages/emacs { inherit pkgs sources; };
  emacsPkg = emacs.emacs-stable;

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix {
    inherit pkgs sources;
    inherit org-babel emacsPkg;
  };

  # services
  basicServices = import ./services/basic.nix;
in
{
  imports = modules ++ basicPrograms ++ basicServices;
  home.stateVersion = "24.11";
  home.packages = basicPkgs;
}
