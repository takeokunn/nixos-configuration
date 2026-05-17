{ pkgs, nurPkgs }:
let
  tig = import ./tig { inherit pkgs nurPkgs; };
in
[
  ./modules/git-hooks
  tig
  { home.packages = [ pkgs.ghq ]; }
]
