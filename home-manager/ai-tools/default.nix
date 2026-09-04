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
    llmAgentsPkgs.terminal-use
    nurPkgs.z_ai-coding-helper
  ];
}
