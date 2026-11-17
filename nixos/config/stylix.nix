{ pkgs, ... }:
{
  stylix = import ../../shared/stylix.nix { inherit pkgs; } // {
    targets.console.enable = true;
    targets.plymouth.enable = true;
  };
  home-manager.sharedModules = [ (import ../../shared/stylix-home-targets.nix) ];
}
