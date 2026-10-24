{
  llmAgentsPkgs,
  ...
}:
{
  imports = [
    ./claude-code
    ./opencode
    ./codex
    ./agent-skills
    ./serena
  ];

  home.packages = [
    llmAgentsPkgs.ccusage
  ];
}
