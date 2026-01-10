{
  pkgs,
  lib,
  username,
  ...
}:
let
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  linuxBuilder = import ./config/linux-builder.nix { inherit lib; };
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix;
  services = import ./config/services;
  system = import ./config/system.nix { inherit username; };
  time = import ./config/time.nix;
  keyboard = import ./config/keyboard.nix;
  startup = import ./config/startup.nix;
  power = import ./config/power.nix;
in
{
  imports = [
    fonts
    homebrew
    linuxBuilder
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
