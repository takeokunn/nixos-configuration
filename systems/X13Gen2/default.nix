{ self, nixpkgs, home-manager, emacs-overlay, ... }:
let
  system = "aarch64-linux";
  username = "take";
in {
  X13Gen2 = nixpkgs.lib.nixosSystem {
    inherit system;

    modules = [
      ./configuration.nix
      home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager { inherit system nixpkgs emacs-overlay; };
      }
    ];
  };
}
