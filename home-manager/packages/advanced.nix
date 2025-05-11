{ pkgs, nodePkgs }:
with pkgs;
[
  yubikey-manager

  # for ai
  ollama
  (aider-chat.overrideAttrs (old: {
    doCheck = false;
  }))
  nodePkgs."@anthropic-ai/claude-code"

  # for web service
  discord
  drawio
  slack
]
