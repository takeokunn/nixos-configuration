{ pkgs, ... }:
{
  stylix = import ../../shared/stylix.nix { inherit pkgs; };
  home-manager.sharedModules = [ (import ../../shared/stylix-home-targets.nix) ];
}
