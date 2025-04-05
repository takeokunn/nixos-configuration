{ pkgs, nodePkgs }:
with pkgs;
[
  # for ai
  ollama
  (aider-chat.withOptional { withAll = true; })
  nodePkgs."@anthropic-ai/claude-code"
  nodePkgs."@modelcontextprotocol/server-brave-search"

  # for web service
  discord
  drawio
  slack
]
