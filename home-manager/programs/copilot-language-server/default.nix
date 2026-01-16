{ pkgs, llmAgentsPkgs }:
{
  home.packages = [
    llmAgentsPkgs.copilot-language-server
  ];

  programs.fish = {
    interactiveShellInit = ''
      set -gx COPILOT_LANGUAGE_SERVER_PATH ${llmAgentsPkgs.copilot-language-server}/bin/copilot-language-server
    '';
  };
}
