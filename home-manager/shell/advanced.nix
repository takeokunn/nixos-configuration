{ pkgs }:
let
  kitty = import ./kitty;
in
[
  kitty
  { home.packages = [ pkgs.yq ]; }
]
