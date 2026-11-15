{ inputs }:
let
  inherit (inputs)
    nixpkgs
    nixos-hardware
    home-manager
    disko
    nixvim
    impermanence
    lanzaboote
    sops-nix
    stylix
    nix-index-database
    ;

  username = "take";
  system = "x86_64-linux";
in
nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = {
    inherit inputs username;
  };

  modules = [
    nixos-hardware.nixosModules.lenovo-thinkpad-x13-amd
    disko.nixosModules.disko
    impermanence.nixosModules.impermanence
    lanzaboote.nixosModules.lanzaboote
    sops-nix.nixosModules.sops
    stylix.nixosModules.stylix
    nix-index-database.nixosModules.nix-index
    home-manager.nixosModules.home-manager

    ../../nixos
    ./hardware-configuration.nix
    ./disko-config.nix
    ./impermanence.nix
    (
      { lib, ... }:
      {
        home-manager.useUserPackages = true;
        home-manager.users."${username}" = {
          imports = [ ../../home-manager/advanced.nix ];
          # This host's home is recreated on every boot, so it tracks the release it was
          # reinstalled on rather than the Mac's.
          home.stateVersion = lib.mkForce "26.11";
        };
        home-manager.sharedModules = [
          nixvim.homeModules.nixvim
          inputs.agent-skills.homeManagerModules.default
          inputs.git-bulk-clean.homeManagerModules.default
        ];
        home-manager.extraSpecialArgs = {
          inherit inputs system;
          inherit (inputs)
            nixpkgs
            nixvim
            mcp-servers-nix
            llm-agents
            cclens
            anthropic-skills
            paredit-cli-skills
            aitools-skills
            yomiyasu-skill
            nur-packages
            emacs-overlay
            org-babel
            ;
        };
      }
    )
  ];
}
