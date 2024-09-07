{ pkgs, sources }:
let
  basic = import ./basic { inherit pkgs sources; };
  denops = import ./denops { inherit pkgs sources; };
  themes = import ./themes { inherit pkgs; };
  ddu = import ./ddu { inherit pkgs sources; };
  ddc = import ./ddc { inherit pkgs sources; };
in basic ++ denops ++ themes ++ ddu ++ ddc
