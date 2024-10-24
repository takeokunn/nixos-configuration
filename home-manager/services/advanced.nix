{ pkgs }:
let
  git-sync = import ./git-sync;
  mako = import ./mako { inherit pkgs; };
  swayidle = import ./swayidle { inherit pkgs; };
  swaync = import ./swaync { inherit pkgs; };
  swayosd = import ./swayosd { inherit pkgs; };
in [ git-sync mako swayidle swaync swayosd ]
