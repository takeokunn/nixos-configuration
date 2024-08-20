{ pkgs }:
with pkgs; [
  # for editor
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))

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
