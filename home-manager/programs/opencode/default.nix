{
  pkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  claude-prompts-path = ../../claude-prompts;
  opencodeConfig = mcp-servers-nix.lib.mkConfig pkgs {
    flavor = "opencode";
    fileName = "opencode.json";

    programs = {
      serena = {
        enable = true;
        context = "claude-code";
        enableWebDashboard = false;
      };
      context7.enable = true;
      playwright.enable = true;
    };

    settings = {
      theme = "dark";
      model = if pkgs.stdenv.isDarwin then "ollama/qwen3.5:27b" else "zai-coding-plan/glm-4.7";

      provider = pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        ollama = {
          npm = "@ai-sdk/openai-compatible";
          options.baseURL = "http://localhost:11434/v1";
          models."qwen3.5:27b" = {
            name = "Qwen3.5";
            tools = true;
            options.num_ctx = 262144;
          };
        };
      };

      servers.deepwiki = {
        type = "http";
        url = "https://mcp.deepwiki.com/mcp";
      };

      permission = {
        bash = {
          "*" = "allow";
          "rm -rf /*" = "deny";
          "rm -rf /" = "deny";
          "sudo rm -rf *" = "deny";
          "dd if=*" = "deny";
          "mkfs.*" = "deny";
          "fdisk *" = "deny";
          "shutdown *" = "deny";
          "reboot *" = "deny";
          "halt *" = "deny";
          "poweroff *" = "deny";
          "killall *" = "deny";
          "pkill -f *" = "deny";
        };
        edit = "allow";
        write = "allow";
        read = "allow";
        glob = "allow";
        grep = "allow";
        webfetch = "allow";
        search = "allow";
        ask = "allow";
        memo = "allow";
        http = "allow";
      };
    };
  };
in
{
  home.file.".opencode/CLAUDE.md" = {
    source = "${claude-prompts-path}/CLAUDE.md";
    force = true;
  };

  xdg.configFile."opencode/opencode.json" = {
    source = opencodeConfig;
    force = true;
  };

  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
    agents = {
      code-quality = builtins.readFile "${claude-prompts-path}/agents/code-quality.md";
      database = builtins.readFile "${claude-prompts-path}/agents/database.md";
      design = builtins.readFile "${claude-prompts-path}/agents/design.md";
      devops = builtins.readFile "${claude-prompts-path}/agents/devops.md";
      docs = builtins.readFile "${claude-prompts-path}/agents/docs.md";
      explore = builtins.readFile "${claude-prompts-path}/agents/explore.md";
      general-purpose = builtins.readFile "${claude-prompts-path}/agents/general-purpose.md";
      git = builtins.readFile "${claude-prompts-path}/agents/git.md";
      performance = builtins.readFile "${claude-prompts-path}/agents/performance.md";
      quality-assurance = builtins.readFile "${claude-prompts-path}/agents/quality-assurance.md";
      security = builtins.readFile "${claude-prompts-path}/agents/security.md";
      test = builtins.readFile "${claude-prompts-path}/agents/test.md";
      validator = builtins.readFile "${claude-prompts-path}/agents/validator.md";
    };
    commands = {
      ask = builtins.readFile "${claude-prompts-path}/commands/ask.md";
      bug = builtins.readFile "${claude-prompts-path}/commands/bug.md";
      define = builtins.readFile "${claude-prompts-path}/commands/define.md";
      define-full = builtins.readFile "${claude-prompts-path}/commands/define-full.md";
      execute = builtins.readFile "${claude-prompts-path}/commands/execute.md";
      execute-full = builtins.readFile "${claude-prompts-path}/commands/execute-full.md";
      feedback = builtins.readFile "${claude-prompts-path}/commands/feedback.md";
      markdown = builtins.readFile "${claude-prompts-path}/commands/markdown.md";
      upstream = builtins.readFile "${claude-prompts-path}/commands/upstream.md";
    };
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
