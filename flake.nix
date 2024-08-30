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
    wezterm-flake = {
      url = "github:wez/wezterm?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
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

  outputs = { self, nixpkgs, nix-darwin, home-manager, emacs-overlay
    , nixos-hardware, xremap, nix-on-droid, wezterm-flake
    , neovim-nightly-overlay, sops-nix, ... }: {
      darwinConfigurations = (import ./systems/OPL2212-2 {
        inherit self nixpkgs nix-darwin home-manager emacs-overlay wezterm-flake
          neovim-nightly-overlay;
      });
      nixosConfigurations = (import ./systems/X13Gen2 {
        inherit self nixpkgs home-manager emacs-overlay nixos-hardware xremap
          wezterm-flake neovim-nightly-overlay sops-nix;
      });
      nixOnDroidConfigurations = (import ./systems/OPPO-A79 {
        inherit self nixpkgs home-manager nix-on-droid;
      });
    };
}
