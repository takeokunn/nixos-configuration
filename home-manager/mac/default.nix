{ pkgs }:
let
  sketchybar = import ./sketchybar { inherit pkgs; };
in
[ sketchybar ]
