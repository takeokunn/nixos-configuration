{ pkgs, devenvPkgs }:
let
  cargo = import ./cargo { inherit pkgs; };
in
[
  ./modules/doggo
  ./modules/lnav
  cargo
  { home.packages = [ devenvPkgs.devenv ]; }
]
