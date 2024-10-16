{ self, nixpkgs, home-manager, org-babel, emacs-overlay, nixos-hardware, xremap
, neovim-nightly-overlay, sops-nix }:
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
      sops-nix.nixosModules.sops
      home-manager.nixosModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" =
          import ../../home-manager/advanced.nix {
            inherit system nixpkgs org-babel emacs-overlay
              neovim-nightly-overlay;
          };
      }
    ];
  };
}
