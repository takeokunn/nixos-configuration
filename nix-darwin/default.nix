{ pkgs, username, ... }:
let
  nix = import ./modules/nix.nix;
  fonts = import ./modules/fonts.nix { inherit pkgs; };
  services = import ./modules/services.nix;
  system = import ./modules/system.nix { inherit pkgs; };
  homebrew = import ./modules/homebrew.nix;
  networking = import ./modules/networking.nix;
  security = import ./modules/security.nix { inherit username; };
  launchd = import ./modules/launchd.nix { inherit pkgs; };
in { inherit nix services fonts system homebrew networking security launchd; }
