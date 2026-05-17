{
  pkgs,
  nurPkgs,
  mcp-servers-nix,
  llmAgentsPkgs,
}:
let
  ai-prompts-path = ../ai-prompts;

  claude-code-fixed = llmAgentsPkgs.claude-code.overrideAttrs (_: {
    doInstallCheck = false;
  });

  readFiles =
    dir: names:
    builtins.listToAttrs (
      map (name: {
        inherit name;
        value = builtins.readFile "${dir}/${name}.md";
      }) names
    );
in
{
  programs.claude-code.enable = true;
  programs.claude-code.package = claude-code-fixed;
  programs.claude-code.context = "${ai-prompts-path}/CLAUDE.md";
  programs.claude-code.settings = {
    theme = "dark";
    model = "sonnet";
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

    statusLine.type = "command";
    statusLine.command = "${ai-prompts-path}/scripts/statusline.sh";
    statusLine.padding = 0;
  };

  programs.claude-code.agents = readFiles "${ai-prompts-path}/agents" [
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
    "verification"
  ];

  programs.claude-code.commands = readFiles "${ai-prompts-path}/commands" [
    "ask"
    "bug"
    "define"
    "define-full"
    "execute"
    "execute-full"
    "feedback"
    "markdown"
    "remember"
    "simplify"
    "skillify"
    "upstream"
  ];

  programs.claude-code.hooks.enforce-perl = builtins.readFile "${ai-prompts-path}/hooks/enforce-perl.sh";
  programs.claude-code.hooks.rtk-rewrite =
    builtins.replaceStrings [ "@RTK_BIN@" ] [ "${llmAgentsPkgs.rtk}/bin/rtk" ]
      (builtins.readFile "${ai-prompts-path}/hooks/rtk-rewrite.sh");

  programs.claude-code.mcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
    }).config.settings.servers
    // {
      serena.command = "${nurPkgs.serena}/bin/serena";
      serena.args = [
        "start-mcp-server"
        "--context"
        "claude-code"
        "--enable-web-dashboard"
        "false"
      ];
      deepwiki.type = "http";
      deepwiki.url = "https://mcp.deepwiki.com/mcp";
    };

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
