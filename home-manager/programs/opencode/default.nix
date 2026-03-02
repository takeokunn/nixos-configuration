{ llmAgentsPkgs }:
let
  claude-prompts-path = ../../claude-prompts;
in
{
  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
    rules = "${claude-prompts-path}/CLAUDE.md";

    settings = {
      model = "anthropic/claude-sonnet-4-5-20250514";

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

    agents = {
      code-quality = builtins.readFile "${claude-prompts-path}/agents/code-quality.md";
      database = builtins.readFile "${claude-prompts-path}/agents/database.md";
      design = builtins.readFile "${claude-prompts-path}/agents/design.md";
      devops = builtins.readFile "${claude-prompts-path}/agents/devops.md";
      docs = builtins.readFile "${claude-prompts-path}/agents/docs.md";
      explore = builtins.readFile "${claude-prompts-path}/agents/explore.md";
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
    MAX_MCP_OUTPUT_TOKENS = "50000";
    MCP_TOOL_TIMEOUT = "120000";
    DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
    DISABLE_TELEMETRY = "1";
    OPENCODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
    OPENCODE_ENABLE_TELEMETRY = "0";
  };

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
