{ pkgs, nodePkgs }:
with pkgs;
[
  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"

  # for web service
  discord
  drawio
  slack
]
