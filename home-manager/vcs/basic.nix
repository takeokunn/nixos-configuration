{ nurPkgs }:
let
  tig = import ./tig { inherit nurPkgs; };
in
[ ./modules/tig ./modules/git-hooks tig ]
