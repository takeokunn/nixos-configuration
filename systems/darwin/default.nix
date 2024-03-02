{ self, nixpkgs, nix-darwin, home-manager, emacs-overlay, ... }:

let
  system = "aarch64-darwin";
  lib = nixpkgs.lib;
  pkgs = import nixpkgs {
    inherit system;
    overlays = [ emacs-overlay.overlay ];
  };
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system lib;
    modules = [
      ./system.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users.obara = import ./home.nix { inherit pkgs; };
      }
    ];
  };
}
