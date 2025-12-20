{
  pkgs,
  mcp-servers-nix,
  nodePkgs,
}:
{
  programs.claude-code = {
    enable = true;
    package = nodePkgs."@anthropic-ai/claude-code";
    memory.source = ./CLAUDE.md;
    settings = {
      theme = "dark";
      autoUpdates = false;
      includeCoAuthoredBy = false;
      autoCompactEnabled = false;
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
      };

      statusLine = {
        type = "command";
        command = ./scripts/statusline.sh;
        padding = 0;
      };

      hooks = {
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "terminal-notifier -message 'ClaudeCodeのタスクが完了しました！' -title 'Claude Code' -sound Blow";
              }
            ];
          }
        ];
      };
    };

    agents = {
      accessibility = builtins.readFile ./agents/accessibility.md;
      api-design = builtins.readFile ./agents/api-design.md;
      architecture = builtins.readFile ./agents/architecture.md;
      ci-cd = builtins.readFile ./agents/ci-cd.md;
      clean = builtins.readFile ./agents/clean.md;
      complexity = builtins.readFile ./agents/complexity.md;
      database = builtins.readFile ./agents/database.md;
      debug = builtins.readFile ./agents/debug.md;
      dependency = builtins.readFile ./agents/dependency.md;
      design = builtins.readFile ./agents/design.md;
      docs = builtins.readFile ./agents/docs.md;
      error-handling = builtins.readFile ./agents/error-handling.md;
      estimation = builtins.readFile ./agents/estimation.md;
      git = builtins.readFile ./agents/git.md;
      i18n = builtins.readFile ./agents/i18n.md;
      infrastructure = builtins.readFile ./agents/infrastructure.md;
      memory = builtins.readFile ./agents/memory.md;
      merge = builtins.readFile ./agents/merge.md;
      migration = builtins.readFile ./agents/migration.md;
      observability = builtins.readFile ./agents/observability.md;
      performance = builtins.readFile ./agents/performance.md;
      playwright = builtins.readFile ./agents/playwright.md;
      quality = builtins.readFile ./agents/quality.md;
      refactor = builtins.readFile ./agents/refactor.md;
      requirement = builtins.readFile ./agents/requirement.md;
      review = builtins.readFile ./agents/review.md;
      security = builtins.readFile ./agents/security.md;
      test = builtins.readFile ./agents/test.md;
    };

    commands = {
      ask = builtins.readFile ./commands/ask.md;
      bug = builtins.readFile ./commands/bug.md;
      define = builtins.readFile ./commands/define.md;
      execute = builtins.readFile ./commands/execute.md;
      markdown = builtins.readFile ./commands/markdown.md;
      feedback = builtins.readFile ./commands/feedback.md;
    };

    mcpServers =
      (mcp-servers-nix.lib.evalModule pkgs {
        programs = {
          context7.enable = true;
          playwright.enable = true;
          terraform.enable = true;
          nixos.enable = true;
          codex.enable = true;
          serena = {
            enable = true;
            args = [
              "--context"
              "ide-assistant"
              "--enable-web-dashboard"
              "False"
            ];
          };
        };
      }).config.settings.servers;
  };
}
