{ pkgs, llmAgentsPkgs }:
{
  home.packages = [
    llmAgentsPkgs.copilot-language-server
  ];

  home.sessionVariables = {
    COPILOT_LANGUAGE_SERVER_PATH = "${llmAgentsPkgs.copilot-language-server}/bin/copilot-language-server";
  };
}
