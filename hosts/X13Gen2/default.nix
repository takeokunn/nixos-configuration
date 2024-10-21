{ inputs }:
let
  inherit (inputs) nixpkgs xremap sops-nix home-manager;
  username = "take";
  system = "x86_64-linux";
in {
  X13Gen2 = nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = { inherit username xremap; };
    modules = [
      ../../nixos
      ./hardware-configuration.nix
      sops-nix.nixosModules.sops
      home-manager.nixosModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager/advanced.nix {
            inherit system;
            inherit (inputs) nixpkgs org-babel emacs-overlay;
          };
      }
    ];
  };
}
