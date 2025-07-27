{ pkgs, nodePkgs }:
with pkgs;
[
  similarity
  yubikey-manager

  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@google/gemini-cli"

  # for web service
  discord
  slack
]
