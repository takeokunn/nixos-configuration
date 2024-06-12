{ config, lib, pkgs }:
let basicPkgs = import ../home-manager/packages/basic.nix { inherit pkgs; };
in {
  home = {
    stateVersion = "24.11";
    packages = basicPkgs;
  };
}
