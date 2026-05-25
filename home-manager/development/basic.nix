{ pkgs, nurPkgs }:
let
  cargo = import ./cargo { inherit pkgs; };
in
[
  ./modules/doggo
  ./modules/lnav
  cargo
  { home.packages = [ nurPkgs.devenv ]; }
]
