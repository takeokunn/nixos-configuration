{ pkgs, nodePkgs }:
with pkgs;
[
  # for ai
  ollama
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@modelcontextprotocol/server-brave-search"

  # for web service
  discord
  drawio
  slack
]
