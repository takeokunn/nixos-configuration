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
    ./gbrain
  ];

  home.packages = [
    llmAgentsPkgs.ccusage
    cclensPkg
  ];
}
