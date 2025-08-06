{ pkgs, nodePkgs }:
with pkgs;
[
  # for lint
  similarity

  # for security
  sops
  yubikey-manager

  # for docker
  lazydocker

  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@google/gemini-cli"

  # for web service
  discord
  slack
]
