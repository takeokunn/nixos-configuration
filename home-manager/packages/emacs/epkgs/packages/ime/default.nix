{ epkgs, nurPkgs }:
let
  nskk = nurPkgs.emacs-nskk;
in
with epkgs;
[
  nskk
]
