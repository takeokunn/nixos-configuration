{ pkgs }:
let
  pandoc = import ./pandoc;
  doggo = import ./doggo;
  lnav = import ./lnav { inherit pkgs; };
in
[ pandoc doggo lnav ]
