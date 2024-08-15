{
  description = "takeokunn's nix system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    xremap.url = "github:xremap/nix-flake";
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-on-droid = {
      url = "github:nix-community/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, emacs-overlay
    , nixos-hardware, xremap, nix-on-droid }: {
      darwinConfigurations = (import ./systems/OPL2212-2 {
        inherit self nixpkgs nix-darwin home-manager emacs-overlay;
      });
      nixosConfigurations = (import ./systems/X13Gen2 {
        inherit self nixpkgs home-manager emacs-overlay nixos-hardware xremap;
      });
      nixOnDroidConfigurations = (import ./systems/OPPO-A79 {
        inherit self nixpkgs home-manager nix-on-droid;
      });
    };
}
