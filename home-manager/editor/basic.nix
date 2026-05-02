{ pkgs, nurPkgs }:
let
  nixvim = import ./nixvim { inherit pkgs nurPkgs; };
  vim = import ./vim { inherit pkgs; };
  editorconfig = import ./editorconfig { inherit pkgs; };
in
[
  nixvim
  vim
  editorconfig
]
