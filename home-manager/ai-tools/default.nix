{
  llmAgentsPkgs,
  ...
}:
{
  imports = [
    ./claude-code
    ./codex
    ./agent-skills
    ./serena
  ];

  home.packages = [
    llmAgentsPkgs.ccusage
  ];
}
