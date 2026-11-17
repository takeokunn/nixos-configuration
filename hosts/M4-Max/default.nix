{ inputs }:
let
  inherit (inputs)
    nix-darwin
    home-manager
    nixpkgs
    mac-app-util
    emacs-overlay
    nur-packages
    ;

  username = "take";
  system = "aarch64-darwin";

  editorOverlay = import ../../home-manager/editor/overlay.nix { inherit emacs-overlay; };
  shellOverlay = import ../../home-manager/shell/overlay.nix;
  sketchybarOverlay = import ../../home-manager/mac/sketchybar/overlay.nix;

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = editorOverlay ++ shellOverlay ++ sketchybarOverlay;
  };

  inherit (import ../../home-manager/editor/emacs-package.nix { inherit pkgs nur-packages; })
    nurPkgs
    emacsLib
    ;
in
nix-darwin.lib.darwinSystem {
  inherit pkgs;
  inherit (nixpkgs) lib;
  specialArgs = {
    inherit
      inputs
      username
      pkgs
      emacsLib
      nurPkgs
      ;
  };
  modules = [
    ../../nix-darwin
    inputs.stylix.darwinModules.stylix
    mac-app-util.darwinModules.default
    home-manager.darwinModules.home-manager
    (import ../home-manager.nix { inherit inputs system username; })
    { home-manager.sharedModules = [ mac-app-util.homeManagerModules.default ]; }
  ];
}
