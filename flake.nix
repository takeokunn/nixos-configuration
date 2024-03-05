{
  description = "takeokunn's nix system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    private-nixpkgs = {
      url = "github:takeokunn/nix-channel";
      flake = false;
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, emacs-overlay, private-nixpkgs, nix-darwin
    , home-manager }: {
      darwinConfigurations = (import ./systems/OPL2212-2 {
        inherit self nixpkgs private-nixpkgs nix-darwin home-manager
          emacs-overlay;
      });
    };
}
