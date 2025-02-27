{ pkgs, nodePkgs }:
with pkgs;
[
  # for ai
  ollama

  # for web service
  discord
  drawio
  slack

  # for node2nix
  nodePkgs."@github/copilot-language-server"
]
