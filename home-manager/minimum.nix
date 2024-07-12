{ pkgs }:
let basicPkgs = import ./packages/basic.nix { inherit pkgs; };
in basicPkgs
