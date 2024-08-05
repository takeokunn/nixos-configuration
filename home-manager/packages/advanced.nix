{ pkgs }:
with pkgs; [
  # for network tools
  speedtest-cli

  # for DB
  mysql

  # for jokes
  asciiquarium
  cmatrix
  fastfetch
  genact
  sl

  # for ai
  ollama

  # for editor
  tree-sitter
  (tree-sitter.withPlugins (p: builtins.attrValues p))
  emacs-git
  pinentry-emacs
  mu
  emacsPackages.mu4e

  # for misc
  discord
  drawio
  slack
]
