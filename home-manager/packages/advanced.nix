{ pkgs, nodePkgs }:
with pkgs;
[
  # lint
  similarity

  # security
  sops
  yubikey-manager

  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@google/gemini-cli"

  # for web service
  discord
  slack
]
