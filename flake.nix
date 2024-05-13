{
  description = "takeokunn's nix system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, emacs-overlay, nixos-hardware }: {
    darwinConfigurations = (import ./systems/OPL2212-2 {
      inherit self nixpkgs nix-darwin home-manager emacs-overlay;
    });
    nixosConfigurations = (import ./systems/X13Gen2 {
      inherit self nixpkgs home-manager emacs-overlay nixos-hardware;
    });
  };
}
