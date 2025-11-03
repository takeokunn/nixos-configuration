{ inputs }:
let
  inherit (inputs) nix-darwin home-manager nixvim;
  inherit (inputs) nixpkgs;

  system = "aarch64-darwin";
  pkgs = import nixpkgs { inherit system; };

  username = "take";
  configuration =
    { ... }:
    {
      users.users.${username}.home = "/Users/${username}";
    };
in
nix-darwin.lib.darwinSystem {
  inherit pkgs system;
  inherit (inputs.nixpkgs) lib;
  specialArgs = {
    inherit username pkgs;
  };
  modules = [
    configuration
    ../../nix-darwin
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
          inherit (inputs) emacs-overlay org-babel;
        };
      };
    }
  ];
}
