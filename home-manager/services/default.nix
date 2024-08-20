{ pkgs }:
let
  nix-gc = import ./nix-gc { inherit pkgs; };
  mako = import ./mako { inherit pkgs; };
in [ nix-gc mako ]
