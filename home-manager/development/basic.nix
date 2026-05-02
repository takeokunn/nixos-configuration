{ pkgs }:
let
  cargo = import ./cargo { inherit pkgs; };
in
[
  ./modules/doggo
  ./modules/lnav
  cargo
]
