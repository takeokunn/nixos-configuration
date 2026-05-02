{ pkgs }:
let
  kitty = import ./kitty { inherit pkgs; };
in
[ kitty ]
