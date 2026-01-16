{ inputs }:
let
  inherit (inputs) nixpkgs xremap;
  inherit (inputs) home-manager disko nixvim;

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
    # ./disko-config.nix
    disko.nixosModules.disko
    home-manager.nixosModules.home-manager
    {
      home-manager.useUserPackages = true;
      home-manager.users."${username}" = import ../../home-manager/advanced.nix;
      home-manager.sharedModules = [
        nixvim.homeModules.nixvim
      ];
      home-manager.extraSpecialArgs = {
        inherit system;
        inherit (inputs) nixpkgs;
        inherit (inputs) nixvim;
        inherit (inputs) mcp-servers-nix;
        inherit (inputs) llm-agents;
        inherit (inputs) emacs-overlay org-babel;
        inherit (inputs) brew-nix arto;
      };
    }
  ];
}
