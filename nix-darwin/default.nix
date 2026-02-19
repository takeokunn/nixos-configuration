{
  pkgs,
  lib,
  username,
  emacsLib,
  ...
}:
let
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix;
  services = import ./config/services { inherit pkgs emacsLib; };
  system = import ./config/system.nix { inherit username; };
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
  ];
}
