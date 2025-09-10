{ nodePkgs }:
{
  xdg.configFile = {
    "claude-code/statusline.sh".source = ./scripts/statusline.sh;
  };

  programs.claude-code = {
    enable = true;
    package = nodePkgs."@anthropic-ai/claude-code";
    settings = {
      theme = "dark";
      model = "opus";
      autoUpdates = false;
      includeCoAuthoredBy = false;
      autoCompactEnabled = false;
      enableAllProjectMcpServers = true;
      enabledMcpjsonServers = [ "context7" ];
      feedbackSurveyState.lastShownTime = 1754089004345;
      outputStyle = "Explanatory";

      permissions = {
        deny = [
          "Bash(rm -rf /*)"
          "Bash(rm -rf /)"
          "Bash(sudo rm -*)"
          "Bash(chmod 777 /*)"
          "Bash(chmod -R 777 /*)"
          "Bash(dd if=*)"
          "Bash(mkfs.*)"
          "Bash(fdisk -*)"
          "Bash(format -*)"
          "Bash(shutdown -*)"
          "Bash(reboot -*)"
          "Bash(halt -*)"
          "Bash(poweroff -*)"
          "Bash(killall -*)"
          "Bash(pkill -*)"
          "Bash(curl * | bash)"
          "Bash(wget * | bash)"
          "Bash(nc -l -*)"
          "Bash(ncat -l -*)"
          "Bash(netcat -l -*)"
          "Bash(rm -rf ~*)"
          "Bash(rm -rf $HOME*)"
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
        command = "~/.config/claude-code/statusline.sh";
      };

      hooks = {
        SessionStart = [
          {
            matcher = "startup|clear";
            hooks = [
              {
                type = "command";
                command = "date -Iseconds";
              }
            ];
          }
        ];
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
  };
}
