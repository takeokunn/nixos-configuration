{ pkgs, username, ... }:
let
  nix = import ./config/nix.nix;
  fonts = import ./config/fonts.nix { inherit pkgs; };
  services = import ./config/services.nix;
  system = import ./config/system.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  security = import ./config/security.nix { inherit username; };
  launchd = import ./config/launchd.nix { inherit pkgs; };
in {
  imports = [ nix services fonts system homebrew networking security launchd ];
}
