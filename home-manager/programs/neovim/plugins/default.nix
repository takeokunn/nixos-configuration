{ pkgs, sources }:
let
  basic = import ./basic { inherit pkgs sources; };
  denops = import ./denops { inherit pkgs sources; };
  themes = import ./themes { inherit pkgs; };
in basic ++ denops ++ themes
