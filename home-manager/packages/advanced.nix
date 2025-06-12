{ pkgs, nodePkgs }:
with pkgs;
[
  yubikey-manager

  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"

  # for web service
  discord
  drawio
  slack
]
