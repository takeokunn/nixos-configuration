{ inputs }:
let
  inherit (inputs) nixpkgs xremap;
  inherit (inputs) sops-nix home-manager;

  username = "take";
  system = "x86_64-linux";
in
nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = {
    inherit username xremap;
  };

  modules = [
    ../../nixos
    ./hardware-configuration.nix
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    {
      home-manager.useUserPackages = true;
      home-manager.sharedModules = [ sops-nix.homeManagerModules.sops ];
      home-manager.users."${username}" = import ../../home-manager/advanced.nix;
      home-manager.extraSpecialArgs = {
        inherit system;
        inherit (inputs) nixpkgs;
        inherit (inputs) mcp-servers-nix;
        inherit (inputs) emacs-overlay org-babel;
      };
    }
  ];
}
