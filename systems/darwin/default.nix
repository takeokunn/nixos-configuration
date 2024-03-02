{ self, nixpkgs, nix-darwin, home-manager, ... }:

let
  system = "aarch64-darwin";
  pkgs = nixpkgs.legacyPackages.${system};
  lib = nixpkgs.lib;
in {
  OPL2212-2 = nix-darwin.lib.darwinSystem {
    inherit system lib;
    modules = [
      ./system.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users.obara = import ./home.nix { inherit pkgs lib; };
      }
    ];
  };
}
