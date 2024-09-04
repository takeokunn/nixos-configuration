{ pkgs }:
let
  basic = import ./basic.nix { inherit pkgs; };
  denops = import ./denops.nix { inherit pkgs; };
  themes = import ./themes.nix { inherit pkgs; };
in basic ++ denops ++ themes
