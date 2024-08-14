{ pkgs, username, ... }:
let
  nix = import ./modules/nix.nix;
  fonts = import ./modules/fonts.nix { inherit pkgs; };
  service = import ./modules/service.nix;
  system = import ./modules/system.nix;
  homebrew = import ./modules/homebrew.nix;
  networking = import ./modules/networking.nix;
  security = import ./modules/security.nix { inherit username; };
  launchd = import ./modules/launchd.nix { inherit pkgs; };
in {
  environment.shells = with pkgs; [ fish ];
  nix = nix;
  services = service;
  fonts = fonts;
  system = system;
  homebrew = homebrew;
  networking = networking;
  security = security;
  launchd = launchd;
}
