{ epkgs }:
let
  grammars = epkgs.treesit-grammars;
  # tree-sitter-cuda: nixpkgs' pinned fetchzip hash for the v0.21.2 archive no
  # longer matches what GitHub serves (hash mismatch, not meta.broken), so it
  # fails the build rather than being filtered out below; drop it explicitly
  # until nixpkgs repins the source.
  excludedGrammars = [ "tree-sitter-cuda" ];
  nonBrokenGrammars = grammars.with-grammars (
    gs:
    builtins.filter (g: !(g.meta.broken or false)) (
      builtins.attrValues (removeAttrs gs excludedGrammars)
    )
  );
in
[
  nonBrokenGrammars
]
