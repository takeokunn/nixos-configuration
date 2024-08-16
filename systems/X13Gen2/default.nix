{ self, nixpkgs, home-manager, emacs-overlay, nixos-hardware, xremap }:
let
  username = "take";
  system = "x86_64-linux";
in {
  X13Gen2 = nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit xremap username; };
    modules = [
      ../../nixos
      ./hardware-configuration.nix
      home-manager.nixosModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager { inherit system nixpkgs emacs-overlay; };
      }
    ];
  };
}
