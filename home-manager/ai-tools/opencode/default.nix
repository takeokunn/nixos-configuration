{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  ai-prompts-path = ../ai-prompts;

  models = import ./oh-my-opencode/models.nix;

  opencodeConfig = import ./opencode-config.nix {
    inherit pkgs mcp-servers-nix nurPkgs;
  };

  ohMyOpencodeConfig = import ./oh-my-opencode {
    inherit pkgs models;
  };

  opencodeAgents = import ./agents.nix { inherit ai-prompts-path; };
in
{
  home.packages = [
    nurPkgs.oh-my-openagent
  ];

  home.file.".opencode/CLAUDE.md".source = "${ai-prompts-path}/CLAUDE.md";
  home.file.".opencode/CLAUDE.md".force = true;

  xdg.configFile."opencode/opencode.json".source = opencodeConfig;
  xdg.configFile."opencode/opencode.json".force = true;

  xdg.configFile."opencode/oh-my-opencode.json".source = ohMyOpencodeConfig;
  xdg.configFile."opencode/oh-my-opencode.json".force = true;

  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
    tui.theme = "dracula";
    tui.scroll_speed = 3;
    tui.scroll_acceleration.enabled = true;
    tui.diff_style = "auto";
    tui.keybinds.messages_half_page_down = "ctrl+d";
    tui.keybinds.messages_half_page_up = "ctrl+u";
    tui.keybinds.messages_next = "]";
    tui.keybinds.messages_previous = "[";
  }
  // opencodeAgents;

  home.sessionVariables = import ./env.nix;

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
