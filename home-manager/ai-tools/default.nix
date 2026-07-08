{
  pkgs,
  nurPkgs,
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
    pkgs.mdq
    llmAgentsPkgs.ccusage
    nurPkgs.z_ai-coding-helper
  ];
}
