{ pkgs }:
let
  kitty = import ./kitty { inherit pkgs; };
in
[
  kitty
  { home.packages = [ pkgs.yq ]; }
]
