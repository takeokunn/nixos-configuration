{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
  modelSet ? "economy",
}:
let
  claude-prompts-path = ../../claude-prompts;

  models = import ./models/${modelSet}.nix;

  opencodeConfig = import ./opencode-config.nix {
    inherit pkgs mcp-servers-nix models;
  };

  tuiConfig = import ./tui-config.nix { inherit pkgs; };

  ohMyOpencodeConfig = import ./oh-my-opencode.nix {
    inherit pkgs models;
  };
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
    agents = builtins.listToAttrs (
      map
        (name: {
          inherit name;
          value = builtins.readFile "${claude-prompts-path}/agents/${name}.md";
        })
        [
          "code-quality"
          "database"
          "design"
          "devops"
          "docs"
          "explore"
          "general-purpose"
          "git"
          "performance"
          "quality-assurance"
          "security"
          "test"
          "validator"
        ]
    );
    commands = builtins.listToAttrs (
      map
        (name: {
          inherit name;
          value = builtins.readFile "${claude-prompts-path}/commands/${name}.md";
        })
        [
          "ask"
          "bug"
          "define"
          "define-full"
          "execute"
          "execute-full"
          "feedback"
          "markdown"
          "upstream"
        ]
    );
  };

  home.sessionVariables = {
    OPENCODE_EXPERIMENTAL_BASH_DEFAULT_TIMEOUT_MS = "300000";
    OPENCODE_EXPERIMENTAL_BASH_MAX_TIMEOUT_MS = "1200000";
    OPENCODE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
    OPENCODE_CODE_MAX_OUTPUT_TOKENS = "32000";
    OPENCODE_CODE_AUTO_CONNECT_IDE = "0";
    OPENCODE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
    OPENCODE_CODE_ENABLE_TELEMETRY = "0";
    OPENCODE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
    OPENCODE_CODE_IDE_SKIP_VALID_CHECK = "1";
    MAX_MCP_OUTPUT_TOKENS = "50000";
    MCP_TOOL_TIMEOUT = "120000";
    DISABLE_AUTOUPDATER = "1";
    DISABLE_ERROR_REPORTING = "1";
    DISABLE_INTERLEAVED_THINKING = "1";
    DISABLE_MICROCOMPACT = "1";
    DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
    DISABLE_TELEMETRY = "1";
    ENABLE_EXPERIMENTAL_MCP_CLI = "false";
    ENABLE_TOOL_SEARCH = "true";
    OPENCODE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
  };

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
