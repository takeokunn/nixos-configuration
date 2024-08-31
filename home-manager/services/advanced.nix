{ pkgs, emacsPkgs }:
let
  mako = import ./mako { inherit pkgs; };
  emacs = import ./emacs { inherit pkgs emacsPkgs; };
in [ mako emacs ]
