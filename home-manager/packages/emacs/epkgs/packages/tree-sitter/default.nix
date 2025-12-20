{ epkgs, pkgs }:
let
  grammars = epkgs.treesit-grammars;
  nonBrokenGrammars = grammars.with-grammars (gs:
    builtins.filter (g: !(g.meta.broken or false)) (builtins.attrValues gs)
  );
in
[
  nonBrokenGrammars
]
