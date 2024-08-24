{ pkgs, xremap, username, ... }:
let
  boot = import ./modules/boot.nix;
  fonts = import ./modules/fonts.nix { inherit pkgs; };
  hardware = import ./modules/hardware.nix;
  i18n = import ./modules/i18n.nix;
  networking = import ./modules/networking.nix;
  nix = import ./modules/nix.nix;
  programs = import ./modules/programs.nix;
  security = import ./modules/security.nix { inherit username; };
  services = import ./modules/services.nix { inherit username; };
  systemd = import ./modules/systemd.nix;
  time = import ./modules/time.nix;
  users = import ./modules/users.nix { inherit pkgs username; };
  virtualisation = import ./modules/virtualisation.nix;
in {
  system.stateVersion = "24.05";
  imports = [
    xremap.nixosModules.default
    boot
    fonts
    hardware
    i18n
    networking
    nix
    programs
    security
    services
    systemd
    time
    users
    virtualisation
  ];
}
