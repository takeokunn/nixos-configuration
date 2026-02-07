{
  pkgs,
  mcp-servers-nix,
  llmAgentsPkgs,
}:
let
  # Override claude-code to disable version check that fails in Nix sandbox
  # due to Bun's Intl.Segmenter ICU initialization issue on macOS
  claude-code-fixed = llmAgentsPkgs.claude-code.overrideAttrs (oldAttrs: {
    doInstallCheck = false;
  });
in
{
  programs.claude-code = {
    enable = true;
    package = claude-code-fixed;
    memory.source = ./CLAUDE.md;
    settings = {
      theme = "dark";
      autoUpdates = false;
      includeCoAuthoredBy = false;
      autoCompactEnabled = true;
      enableAllProjectMcpServers = true;
      feedbackSurveyState.lastShownTime = 1754089004345;
      outputStyle = "Explanatory";

      permissions = {
        deny = [
          "Bash(rm -rf /*)"
          "Bash(rm -rf /)"
          "Bash(sudo rm -:*)"
          "Bash(chmod 777 /*)"
          "Bash(chmod -R 777 /*)"
          "Bash(dd if=:*)"
          "Bash(mkfs.:*)"
          "Bash(fdisk -:*)"
          "Bash(format -:*)"
          "Bash(shutdown -:*)"
          "Bash(reboot -:*)"
          "Bash(halt -:*)"
          "Bash(poweroff -:*)"
          "Bash(killall -:*)"
          "Bash(pkill -:*)"
          "Bash(nc -l -:*)"
          "Bash(ncat -l -:*)"
          "Bash(netcat -l -:*)"
          "Bash(rm -rf ~:*)"
          "Bash(rm -rf $HOME:*)"
          "Bash(rm -rf ~/.ssh*)"
          "Bash(rm -rf ~/.config*)"
        ];
      };

      env = {
        BASH_DEFAULT_TIMEOUT_MS = "300000";
        BASH_MAX_TIMEOUT_MS = "1200000";
        CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
        MAX_MCP_OUTPUT_TOKENS = "50000";
        MCP_TOOL_TIMEOUT = "120000";
        CLAUDE_CODE_MAX_OUTPUT_TOKENS = "32000";
        CLAUDE_CODE_AUTO_CONNECT_IDE = "0";
        CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
        CLAUDE_CODE_ENABLE_TELEMETRY = "0";
        CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
        CLAUDE_CODE_IDE_SKIP_VALID_CHECK = "1";
        DISABLE_AUTOUPDATER = "1";
        DISABLE_ERROR_REPORTING = "1";
        DISABLE_INTERLEAVED_THINKING = "1";
        DISABLE_MICROCOMPACT = "1";
        DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
        DISABLE_TELEMETRY = "1";
        ENABLE_EXPERIMENTAL_MCP_CLI = "false";
        ENABLE_TOOL_SEARCH = "true";
        CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
      };

      statusLine = {
        type = "command";
        command = ./scripts/statusline.sh;
        padding = 0;
      };

    };

    agents = {
      code-quality = builtins.readFile ./agents/code-quality.md;
      database = builtins.readFile ./agents/database.md;
      design = builtins.readFile ./agents/design.md;
      devops = builtins.readFile ./agents/devops.md;
      docs = builtins.readFile ./agents/docs.md;
      explore = builtins.readFile ./agents/explore.md;
      git = builtins.readFile ./agents/git.md;
      performance = builtins.readFile ./agents/performance.md;
      quality-assurance = builtins.readFile ./agents/quality-assurance.md;
      security = builtins.readFile ./agents/security.md;
      test = builtins.readFile ./agents/test.md;
      validator = builtins.readFile ./agents/validator.md;
    };

    commands = {
      ask = builtins.readFile ./commands/ask.md;
      bug = builtins.readFile ./commands/bug.md;
      define = builtins.readFile ./commands/define.md;
      define-full = builtins.readFile ./commands/define-full.md;
      execute = builtins.readFile ./commands/execute.md;
      execute-full = builtins.readFile ./commands/execute-full.md;
      feedback = builtins.readFile ./commands/feedback.md;
      markdown = builtins.readFile ./commands/markdown.md;
      upstream = builtins.readFile ./commands/upstream.md;
    };

    hooks = {
      enforce-perl = builtins.readFile ./hooks/enforce-perl.sh;
    };

    skills = {
      # patterns
      core-patterns = builtins.readFile ./skills/patterns/core-patterns/SKILL.md;
      parallelization-patterns = builtins.readFile ./skills/patterns/parallelization-patterns/SKILL.md;
      workflow-patterns = builtins.readFile ./skills/patterns/workflow-patterns/SKILL.md;

      # tools
      serena-usage = builtins.readFile ./skills/tools/serena-usage/SKILL.md;
      context7-usage = builtins.readFile ./skills/tools/context7-usage/SKILL.md;
      exploration-tools = builtins.readFile ./skills/tools/exploration-tools/SKILL.md;
      quality-tools = builtins.readFile ./skills/tools/quality-tools/SKILL.md;

      # methodology
      requirements-definition = builtins.readFile ./skills/methodology/requirements-definition/SKILL.md;
      define-core = builtins.readFile ./skills/methodology/define-core/SKILL.md;
      investigation-patterns = builtins.readFile ./skills/methodology/investigation-patterns/SKILL.md;
      execution-workflow = builtins.readFile ./skills/methodology/execution-workflow/SKILL.md;
      fact-check = builtins.readFile ./skills/methodology/fact-check/SKILL.md;
      testing-patterns = builtins.readFile ./skills/methodology/testing-patterns/SKILL.md;
      technical-documentation = builtins.readFile ./skills/methodology/technical-documentation/SKILL.md;
      technical-writing = builtins.readFile ./skills/methodology/technical-writing/SKILL.md;

      # ecosystem
      nix-ecosystem = builtins.readFile ./skills/ecosystem/nix-ecosystem/SKILL.md;
      typescript-ecosystem = builtins.readFile ./skills/ecosystem/typescript-ecosystem/SKILL.md;
      golang-ecosystem = builtins.readFile ./skills/ecosystem/golang-ecosystem/SKILL.md;
      rust-ecosystem = builtins.readFile ./skills/ecosystem/rust-ecosystem/SKILL.md;
      common-lisp-ecosystem = builtins.readFile ./skills/ecosystem/common-lisp-ecosystem/SKILL.md;
      emacs-ecosystem = builtins.readFile ./skills/ecosystem/emacs-ecosystem/SKILL.md;
      org-ecosystem = builtins.readFile ./skills/ecosystem/org-ecosystem/SKILL.md;
      aws-ecosystem = builtins.readFile ./skills/ecosystem/aws-ecosystem/SKILL.md;
      cplusplus-ecosystem = builtins.readFile ./skills/ecosystem/cplusplus-ecosystem/SKILL.md;
      sql-ecosystem = builtins.readFile ./skills/ecosystem/sql-ecosystem/SKILL.md;
      c-ecosystem = builtins.readFile ./skills/ecosystem/c-ecosystem/SKILL.md;
      php-ecosystem = builtins.readFile ./skills/ecosystem/php-ecosystem/SKILL.md;
      swift-ecosystem = builtins.readFile ./skills/ecosystem/swift-ecosystem/SKILL.md;
      haskell-ecosystem = builtins.readFile ./skills/ecosystem/haskell-ecosystem/SKILL.md;
      devenv-ecosystem = builtins.readFile ./skills/ecosystem/devenv-ecosystem/SKILL.md;
    };

    mcpServers =
      (mcp-servers-nix.lib.evalModule pkgs {
        programs = {
          context7.enable = true;
          serena = {
            enable = true;
            context = "claude-code";
            enableWebDashboard = false;
          };
        };
      }).config.settings.servers;
  };
}
