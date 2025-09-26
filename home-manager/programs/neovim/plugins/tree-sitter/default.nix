{ pkgs }:
with pkgs.vimPlugins;
[
  nvim-treesitter.withAllGrammars
]
