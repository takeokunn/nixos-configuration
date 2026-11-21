{ pkgs, inputs, ... }:
let
  inherit (import inputs.nur-packages { inherit pkgs; }) dracula-wallpaper;
in
{
  stylix = import ../../shared/stylix.nix { inherit pkgs; } // {
    # Shared with the lock and login screens; awww reads the same path as its default wallpaper.
    image = "${dracula-wallpaper}/first-collection/nixos.png";
    cursor = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
      size = 24;
    };
    icons = {
      enable = true;
      package = pkgs.dracula-icon-theme;
      dark = "Dracula";
      light = "Dracula";
    };
    # Kept out of shared/stylix.nix: the kitty target turns opacity.terminal into
    # background_opacity, which would make the Mac terminal translucent too.
    opacity = {
      terminal = 0.94;
      popups = 0.95;
    };
    targets.console.enable = true;
    targets.plymouth.enable = true;
    # Colors the Chrome and Chromium frame through managed policy (replaces the Dracula theme extension).
    targets.chromium.enable = true;
  };
  home-manager.sharedModules = [ (import ../../shared/stylix-home-targets.nix) ];
}
