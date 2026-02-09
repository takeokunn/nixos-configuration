{ inputs }:
let
  inherit (inputs) nixpkgs xremap nixos-hardware;
  inherit (inputs)
    home-manager
    disko
    nixvim
    impermanence
    ;

  username = "take";
  system = "x86_64-linux";
in
nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = {
    inherit username xremap;
  };

  modules = [
    # External modules
    nixos-hardware.nixosModules.lenovo-thinkpad-x13-amd
    disko.nixosModules.disko
    impermanence.nixosModules.impermanence
    home-manager.nixosModules.home-manager

    # Local configuration
    ../../nixos
    ./hardware-configuration.nix
    ./disko-config.nix
    ./impermanence.nix
    {
      # LUKS configuration
      boot.initrd.luks.devices."cryptroot" = {
        device = "/dev/disk/by-partlabel/cryptroot";
        allowDiscards = true;
        bypassWorkqueues = true;
      };

      # Enable systemd in initrd for LUKS and impermanence
      boot.initrd.systemd.enable = true;
    }
    {
      home-manager.useUserPackages = true;
      home-manager.users."${username}" = import ../../home-manager/advanced.nix;
      home-manager.sharedModules = [
        nixvim.homeModules.nixvim
        inputs.zen-browser.homeModules.twilight
        inputs.agent-skills.homeManagerModules.default
      ];
      home-manager.extraSpecialArgs = {
        inherit inputs system;
        inherit (inputs)
          nixpkgs
          nixvim
          mcp-servers-nix
          llm-agents
          anthropic-skills
          cloudflare-skills
          hashicorp-agent-skills
          nur-packages
          emacs-overlay
          org-babel
          brew-nix
          firefox-addons
          ;
      };
    }
  ];
}
