{ system, nixpkgs, emacs-overlay, ... }:
let
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = import ./overlay.nix { inherit emacs-overlay; };
  };
in {
  home.stateVersion = "23.11";
  home.packages = import ./package.nix { inherit pkgs; };
}
