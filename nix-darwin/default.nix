{ pkgs, username, ... }:
let
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix { inherit username; };
  services = import ./config/services;
  system = import ./config/system.nix { inherit pkgs username; };
  time = import ./config/time.nix;
  keyboard = import ./config/keyboard.nix;
  startup = import ./config/startup.nix;
  power = import ./config/power.nix;
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
    keyboard
    startup
    power
  ];
}
