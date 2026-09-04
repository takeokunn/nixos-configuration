{
  config,
  pkgs,
  nurPkgs,
  mcp-servers-nix,
  llmAgentsPkgs,
  guardAndGuide,
  ...
}:
let
  ai-prompts-path = ../ai-prompts;

  shared = import ../shared { inherit (pkgs) lib; };

  hooksDir = "${config.programs.claude-code.configDir}/hooks";

  # Derived from the shared catalog rather than restated, so a guardrail added there for Codex
  # cannot silently fail to fire here. enforce-perl is the one deliberate omission: guard-and-guide
  # carries its sed/awk rule for Claude Code, while Codex still wires the script itself.
  claudeBashHookNames = builtins.filter (n: n != "enforce-perl") shared.guardrailHookNames ++ [
    "rtk-rewrite"
  ];

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
  programs.claude-code.context = builtins.readFile "${ai-prompts-path}/CLAUDE.md";
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
      deny = map (p: "Bash(${p})") shared.bashDenyPatterns;
    };

    env = {
      BASH_DEFAULT_TIMEOUT_MS = "300000";
      BASH_MAX_TIMEOUT_MS = "1200000";
      CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
      MAX_MCP_OUTPUT_TOKENS = "25000";
      MCP_TOOL_TIMEOUT = "120000";
      CLAUDE_CODE_MAX_OUTPUT_TOKENS = "32000";
      CLAUDE_CODE_SUBAGENT_MODEL = "sonnet";
      CLAUDE_CODE_AUTO_CONNECT_IDE = "0";
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
      CLAUDE_CODE_ENABLE_TELEMETRY = "0";
      CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
      CLAUDE_CODE_IDE_SKIP_VALID_CHECK = "1";
      DISABLE_AUTOUPDATER = "1";
      DISABLE_ERROR_REPORTING = "1";
      DISABLE_INTERLEAVED_THINKING = "1";
      DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
      DISABLE_TELEMETRY = "1";
      ENABLE_EXPERIMENTAL_MCP_CLI = "false";
      ENABLE_TOOL_SEARCH = "true";
      CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
    };

    # Claude Code fans one event out to every matching hook in parallel and merges the results
    # afterwards, so position in this list confers nothing: no hook runs before another and none
    # sees another's updatedInput. Every hook here judges the command as issued. rtk-rewrite is
    # currently the only one emitting updatedInput — adding a second would make precedence between
    # them non-deterministic, and this list could not resolve it.
    hooks.PreToolUse = [
      # The empty matcher is what reaches Read, Write, and Edit; the Bash entry below cannot see
      # them, which is the gap guard-and-guide was added to close.
      {
        matcher = "";
        hooks = [
          {
            type = "command";
            command = "${guardAndGuide}/bin/guard-and-guide --config ${ai-prompts-path}/hooks/rules.toml";
          }
        ];
      }
      {
        matcher = "Bash";
        # The assert fails the build if the derived wiring stops matching what this file expects —
        # a guardrail renamed or added in the shared catalog would otherwise be installed and never
        # fire, which has gone unnoticed here once before.
        hooks =
          assert
            claudeBashHookNames == [
              "block-destructive-git"
              "block-bare-cd"
              "rtk-rewrite"
            ];
          map (name: {
            type = "command";
            command = "${hooksDir}/${name}";
          }) claudeBashHookNames;
      }
    ];

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
    "execute"
    "execute-full"
    "markdown"
    "upstream"
  ];

  programs.claude-code.hooks.block-destructive-git = builtins.readFile "${ai-prompts-path}/hooks/block-destructive-git.sh";
  programs.claude-code.hooks.block-bare-cd = builtins.readFile "${ai-prompts-path}/hooks/block-bare-cd.sh";
  programs.claude-code.hooks.rtk-rewrite =
    builtins.replaceStrings [ "@RTK_BIN@" ] [ "${llmAgentsPkgs.rtk}/bin/rtk" ]
      (builtins.readFile "${ai-prompts-path}/hooks/rtk-rewrite.sh");

  programs.claude-code.mcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
      programs.slite.enable = true;
      programs.clickup.enable = true;
    }).config.settings.servers
    // shared.mcpServers { inherit nurPkgs; };
}
