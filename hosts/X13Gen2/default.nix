{ inputs }:
let
  inherit (inputs)
    nixpkgs
    nixos-hardware
    home-manager
    disko
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
    (import ../home-manager.nix { inherit inputs system username; })
    (
      { lib, ... }:
      {
        # This host's home is recreated on every boot, so it tracks the release it was
        # reinstalled on rather than the Mac's.
        home-manager.users.${username}.home.stateVersion = lib.mkForce "26.11";
      }
    )
  ];
}
