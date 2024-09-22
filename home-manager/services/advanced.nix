{ pkgs }:
let
  mako = import ./mako { inherit pkgs; };
  swayidle = import ./swayidle { inherit pkgs; };
  swaync = import ./swaync { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
in [ mako swayidle swaync swayosd ]
