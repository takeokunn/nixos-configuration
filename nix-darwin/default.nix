{ pkgs, username, ... }:
let
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix { inherit username; };
  services = import ./config/services;
  system = import ./config/system.nix { inherit pkgs; };
  time = import ./config/time.nix;
in
{
  imports = [
    fonts
    homebrew
    networking
    nix
    security
    services
    system
    time
  ];
}
