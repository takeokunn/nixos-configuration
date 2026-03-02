{ llmAgentsPkgs }:
{
  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
  };
}
