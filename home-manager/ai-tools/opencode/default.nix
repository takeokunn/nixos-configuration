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
    inherit pkgs mcp-servers-nix nurPkgs;
  };

  ohMyOpencodeConfig = import ./oh-my-opencode {
    inherit pkgs models;
  };

  opencodeAgents = import ./agent-translation.nix { inherit pkgs ai-prompts-path; };
in
{
  # oh-my-openagent evaluates to null on aarch64-linux (darwin-only in the
  # NUR), so guard it for the Linux containers.
  home.packages = pkgs.lib.optionals (nurPkgs.oh-my-openagent != null) [
    nurPkgs.oh-my-openagent
  ];

  home.file.".opencode/CLAUDE.md".source = "${ai-prompts-path}/CLAUDE.md";
  home.file.".opencode/CLAUDE.md".force = true;

  xdg.configFile."opencode/opencode.json".source = opencodeConfig;

  xdg.configFile."opencode/oh-my-opencode.json".source = ohMyOpencodeConfig;

  # programs.opencode.agents/commands dispatches on builtins.isPath at the
  # module's implementation, not on the option's declared `either` type: a
  # derivation (e.g. from pkgs.linkFarm) is builtins.isAttrs, so it falls
  # into the attrset-of-content branch and mapAttrs' over the derivation's
  # own attrs (outPath, drvPath, ...) instead of being symlinked whole.
  # xdg.configFile.recursive sidesteps that dispatch entirely.
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

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
