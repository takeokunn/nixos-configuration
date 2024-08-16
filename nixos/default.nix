{ config, pkgs, xremap, username, system, ... }:
let
  boot = import ./modules/boot.nix;
  networking = import ./modules/networking.nix;
  time = import ./modules/time.nix;
  i18n = import ./modules/i18n.nix;
  hardware = import ./modules/hardware.nix;
  programs = import ./modules/programs.nix;
  services = import ./modules/services.nix { inherit username; };
  systemd = import ./modules/systemd.nix;
  security = import ./modules/security.nix;
  users = import ./modules/users.nix { inherit pkgs username; };
  fonts = import ./modules/fonts.nix { inherit pkgs; };
in {
  imports = [ xremap.nixosModules.default ];
  system.stateVersion = "24.05";

  boot = boot;
  networking = networking;
  time = time;
  i18n = i18n;
  hardware = hardware;
  programs = programs;
  services = services;
  systemd = systemd;
  security = security;
  users = users;
  fonts = fonts;
}
