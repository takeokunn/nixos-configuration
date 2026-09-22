{
  inputs,
  pkgs,
  username,
  emacsLib,
  nurPkgs,
  ...
}:
let
  claudeCodeSettings = import ./config/claude-code-settings.nix {
    inherit pkgs username;
    guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix { inherit username; };
  services = import ./config/services { inherit emacsLib; };
  spotlight = import ./config/spotlight.nix;
  system = import ./config/system.nix { inherit username; };
  wallpaper = import ./config/wallpaper.nix { inherit nurPkgs username; };
in
{
  imports = [
    claudeCodeSettings
    fonts
    homebrew
    networking
    nix
    security
    services
    spotlight
    system
    wallpaper
  ];
}
