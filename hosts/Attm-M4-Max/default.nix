{ inputs }:
let
  inherit (inputs)
    nix-darwin
    home-manager
    nixvim
    brew-nix
    ;
  inherit (inputs) nixpkgs;

  username = "take";
  system = "aarch64-darwin";

  pkgs = import nixpkgs {
    inherit system;
    overlays = [ brew-nix.overlays.default ];
  };

  configuration =
    { ... }:
    {
      users.users.${username}.home = "/Users/${username}";
    };
in
nix-darwin.lib.darwinSystem {
  inherit pkgs;
  inherit (inputs.nixpkgs) lib;
  specialArgs = {
    inherit username pkgs;
  };
  modules = [
    configuration
    ../../nix-darwin
    brew-nix.darwinModules.default
    home-manager.darwinModules.home-manager
    {
      home-manager = {
        useUserPackages = true;
        users."${username}" = import ../../home-manager/advanced.nix;
        sharedModules = [ nixvim.homeModules.nixvim ];
        extraSpecialArgs = {
          inherit system username;
          inherit (inputs) nixpkgs nixvim;
          inherit (inputs) mcp-servers-nix;
          inherit (inputs) emacs-overlay org-babel brew-nix;
        };
      };
    }
  ];
}
