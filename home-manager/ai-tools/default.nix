{
  llmAgentsPkgs,
  cclensPkg,
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
    cclensPkg
  ];
}
