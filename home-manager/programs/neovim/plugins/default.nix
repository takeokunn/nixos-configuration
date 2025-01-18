{ pkgs, sources }:
let
  basic = import ./basic { inherit pkgs sources; };
  git = import ./git { inherit pkgs sources; };
  language = import ./language { inherit pkgs sources; };
  lsp = import ./lsp { inherit pkgs; };
  skk = import ./skk { inherit pkgs sources; };
  themes = import ./themes { inherit pkgs; };
  telescope = import ./telescope { inherit pkgs; };
in
basic ++ git ++ language ++ lsp ++ skk ++ themes ++ telescope
