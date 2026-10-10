{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
  ...
}:
let
  ai-prompts-path = ../ai-prompts;

  models = import ./oh-my-opencode/models.nix;

  opencodeConfig = import ./opencode-config.nix {
    inherit
      pkgs
      mcp-servers-nix
      nurPkgs
      models
      ;
  };

  ohMyOpencodeConfig = import ./oh-my-opencode {
    inherit pkgs models;
  };

  opencodeAgents = import ./agent-translation.nix { inherit pkgs ai-prompts-path; };

  aitoolsRewrite = pkgs.writeShellScript "aitools-rewrite" (
    builtins.readFile "${ai-prompts-path}/hooks/aitools-rewrite.sh"
  );
in
{
  home.packages = pkgs.lib.optionals (nurPkgs.oh-my-openagent != null) [
    nurPkgs.oh-my-openagent
  ];

  home.file.".opencode/CLAUDE.md".source = "${ai-prompts-path}/CLAUDE.md";
  home.file.".opencode/CLAUDE.md".force = true;

  xdg.configFile."opencode/opencode.json".source = opencodeConfig;

  xdg.configFile."opencode/oh-my-opencode.json".source = ohMyOpencodeConfig;

  # A single file, not the directory: other tools drop their own plugins into `plugin/`.
  xdg.configFile."opencode/plugin/aitools-rewrite.js".text =
    builtins.replaceStrings [ "@AITOOLS_REWRITE@" ] [ "${aitoolsRewrite}" ]
      (builtins.readFile ./aitools-rewrite-plugin.js);

  xdg.configFile."opencode/agents" = {
    source = opencodeAgents.agents;
    recursive = true;
  };
  xdg.configFile."opencode/commands" = {
    source = opencodeAgents.commands;
    recursive = true;
  };

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
  };

  home.sessionVariables = import ./env.nix;
}
