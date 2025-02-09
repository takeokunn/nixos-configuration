{ inputs }:
let
  inherit (inputs)
    nixpkgs
    xremap
    sops-nix
    home-manager
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
    {
      sops = {
        defaultSopsFile = ../../secrets/password.yaml;
        age.sshKeyPaths = [ "/home/take/.ssh/id_ed25519" ];
        secrets = {
          home-wifi = { };
        };
      };
    }
    ../../nixos
    ./hardware-configuration.nix
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    {
      home-manager.useUserPackages = true;
      home-manager.users."${username}" = import ../../home-manager/advanced.nix {
        inherit system;
        inherit (inputs) nixpkgs org-babel emacs-overlay;
      };
    }
  ];
}
