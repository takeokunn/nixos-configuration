{ pkgs, neovim-skkeleton-flake }:
with pkgs; [
  # for editor
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  neovim-skkeleton-flake.packages.${pkgs.system}.default

  # for network tools
  speedtest-cli

  # for DB
  mysql

  # for ai
  ollama

  # for misc
  discord
  drawio
  slack
]
