{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  claude-prompts-path = ../../claude-prompts;

  models = import ./oh-my-opencode/models.nix;

  opencodeConfig = import ./opencode-config.nix {
    inherit pkgs mcp-servers-nix;
  };

  tuiConfig = import ./tui-config.nix { inherit pkgs; };

  ohMyOpencodeConfig = import ./oh-my-opencode {
    inherit pkgs models;
  };

  opencodeAgents = import ./agents.nix { inherit claude-prompts-path; };
in
{
  home.packages = [
    nurPkgs.oh-my-openagent
  ];

  home.file.".opencode/CLAUDE.md" = {
    source = "${claude-prompts-path}/CLAUDE.md";
    force = true;
  };

  xdg.configFile."opencode/opencode.json" = {
    source = opencodeConfig;
    force = true;
  };

  xdg.configFile."opencode/oh-my-opencode.json" = {
    source = ohMyOpencodeConfig;
    force = true;
  };

  xdg.configFile."opencode/tui.json" = {
    source = tuiConfig;
    force = true;
  };

  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
  } // opencodeAgents;

  home.sessionVariables = import ./env.nix;

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
