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

  opencodeSettings = import ./opencode-config.nix {
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

  opencodeAgents = import ./agent-translation.nix {
    inherit (pkgs) lib;
    inherit ai-prompts-path;
  };

  aitoolsRewrite = pkgs.writeShellScript "aitools-rewrite" (
    builtins.readFile "${ai-prompts-path}/hooks/aitools-rewrite.sh"
  );
in
{
  home.packages = pkgs.lib.optionals (nurPkgs.oh-my-openagent != null) [
    nurPkgs.oh-my-openagent
  ];

  xdg.configFile."opencode/oh-my-opencode.json".source = ohMyOpencodeConfig;

  # A single file, not the directory: other tools drop their own plugins into the plugin directories.
  xdg.configFile."opencode/plugins/aitools-rewrite.js".text =
    builtins.replaceStrings [ "@AITOOLS_REWRITE@" ] [ "${aitoolsRewrite}" ]
      (builtins.readFile ./aitools-rewrite-plugin.js);

  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
    # The package exposes no passthru.jsonschema, so validateFiles.config cannot be enabled.
    enableMcpIntegration = false;
    settings = opencodeSettings;
    context = ai-prompts-path + "/CLAUDE.md";
    inherit (opencodeAgents) agents commands;
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
