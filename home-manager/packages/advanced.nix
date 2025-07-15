{ pkgs, nodePkgs }:
with pkgs;
[
  yubikey-manager

  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@google/gemini-cli"

  # for web service
  discord
  drawio
  slack
]
