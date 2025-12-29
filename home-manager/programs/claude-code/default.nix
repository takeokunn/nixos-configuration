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
      code-quality = builtins.readFile ./agents/code-quality.md;
      database = builtins.readFile ./agents/database.md;
      design = builtins.readFile ./agents/design.md;
      devops = builtins.readFile ./agents/devops.md;
      docs = builtins.readFile ./agents/docs.md;
      git = builtins.readFile ./agents/git.md;
      performance = builtins.readFile ./agents/performance.md;
      quality-assurance = builtins.readFile ./agents/quality-assurance.md;
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

    hooks = {
      enforce-perl = builtins.readFile ./hooks/enforce-perl.sh;
    };

    skills = {
      requirements-definition = builtins.readFile ./skills/requirements-definition/SKILL.md;
      investigation-patterns = builtins.readFile ./skills/investigation-patterns/SKILL.md;
      execution-workflow = builtins.readFile ./skills/execution-workflow/SKILL.md;
      nix-ecosystem = builtins.readFile ./skills/nix-ecosystem/SKILL.md;
      typescript-ecosystem = builtins.readFile ./skills/typescript-ecosystem/SKILL.md;
      golang-ecosystem = builtins.readFile ./skills/golang-ecosystem/SKILL.md;
      rust-ecosystem = builtins.readFile ./skills/rust-ecosystem/SKILL.md;
      common-lisp-ecosystem = builtins.readFile ./skills/common-lisp-ecosystem/SKILL.md;
      emacs-ecosystem = builtins.readFile ./skills/emacs-ecosystem/SKILL.md;
      testing-patterns = builtins.readFile ./skills/testing-patterns/SKILL.md;
      serena-usage = builtins.readFile ./skills/serena-usage/SKILL.md;
      context7-usage = builtins.readFile ./skills/context7-usage/SKILL.md;
      technical-documentation = builtins.readFile ./skills/technical-documentation/SKILL.md;
      technical-writing = builtins.readFile ./skills/technical-writing/SKILL.md;
    };

    mcpServers =
      (mcp-servers-nix.lib.evalModule pkgs {
        programs = {
          context7.enable = true;
          codex.enable = true;
          serena = {
            enable = true;
            context = "claude-code";
            enableWebDashboard = false;
          };
        };
      }).config.settings.servers;
  };
}
