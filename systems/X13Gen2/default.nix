{ self, nixpkgs, home-manager, org-babel, emacs-overlay, emacs-flake
, nixos-hardware, xremap, wezterm-flake, neovim-nightly-overlay }:
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
        home-manager.users."${username}" = import ../../home-manager {
          inherit system nixpkgs org-babel emacs-overlay emacs-flake
            wezterm-flake neovim-nightly-overlay;
        };
      }
    ];
  };
}
