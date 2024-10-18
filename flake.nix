{
  description = "takeokunn's nix system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    org-babel.url = "github:emacs-twist/org-babel";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    xremap = {
      url = "github:xremap/nix-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-on-droid = {
      url = "github:nix-community/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, org-babel, emacs-overlay
    , nixos-hardware, xremap, nix-on-droid, neovim-nightly-overlay, sops-nix
    , ... }: {
      darwinConfigurations = (import ./hosts/OPL2212-2 {
        inherit self nixpkgs nix-darwin home-manager org-babel emacs-overlay
          neovim-nightly-overlay;
      });
      nixosConfigurations = (import ./hosts/X13Gen2 {
        inherit self nixpkgs home-manager org-babel emacs-overlay nixos-hardware
          xremap neovim-nightly-overlay sops-nix;
      });
      nixOnDroidConfigurations = (import ./hosts/OPPO-A79 {
        inherit self nixpkgs home-manager nix-on-droid;
      });
    };
}
