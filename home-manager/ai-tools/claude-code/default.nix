{
  config,
  pkgs,
  nurPkgs,
  mcp-servers-nix,
  llmAgentsPkgs,
  ...
}:
let
  ai-prompts-path = ../ai-prompts;

  hooksDir = "${config.programs.claude-code.configDir}/hooks";

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
  # readFile, not "${...}": the option type is `either lines path`, and interpolating a
  # path yields a string, which is accepted as the content itself. That wrote the store
  # path into CLAUDE.md instead of the prompt.
  programs.claude-code.context = builtins.readFile "${ai-prompts-path}/CLAUDE.md";
  programs.claude-code.settings = {
    theme = "dark";
    model = "sonnet";
    autoUpdates = false;
    includeCoAuthoredBy = false;
    autoCompactEnabled = true;
    # Left unset the window is a model-tuned default, which for Sonnet 5 on this entrypoint is
    # 967000. Cache-read per request grows with session length until compaction caps it, and the
    # long sessions that reach that cap carry most of a week's tokens, so the cap is the lever.
    # Re-derive rather than trusting a remembered figure: `ccusage claude weekly --json` for the
    # totals, and a per-turn scan of ~/.claude/projects/**/*.jsonl for the growth curve — noting
    # that one assistant turn spans several lines sharing a message id, and only the line with the
    # largest output_tokens may be counted.
    #
    # This value is the ceiling, not the trigger. Claude Code subtracts the output reserve and a
    # further margin, so compaction actually arms below it and fires below that; statusline.sh
    # carries the derived numbers and has to move whenever this one does.
    autoCompactWindow = 200000;
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
      # Back to the documented default. A single oversized MCP response does not cost once:
      # it stays in the transcript and is re-read on every later request in the session.
      MAX_MCP_OUTPUT_TOKENS = "25000";
      MCP_TOOL_TIMEOUT = "120000";
      CLAUDE_CODE_MAX_OUTPUT_TOKENS = "32000";
      # A sub-agent whose definition omits `model` inherits the parent's, so delegating from an
      # Opus session runs every child on Opus. There is no settings.json key for this; the
      # environment variable is the only global override, and it outranks both the Agent tool's
      # model argument and the agent file's frontmatter.
      #
      # Because `model` above is already sonnet, this is inert for a default session. It bites
      # exactly when the model is raised by hand: the parent becomes Opus and the children stay
      # here. Pass an explicit model to the Agent tool for a child that genuinely needs the
      # larger one. To confirm the alias resolves rather than assuming it, read message.model in
      # a freshly spawned sub-agent's transcript under ~/.claude/projects.
      CLAUDE_CODE_SUBAGENT_MODEL = "sonnet";
      CLAUDE_CODE_AUTO_CONNECT_IDE = "0";
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
      CLAUDE_CODE_ENABLE_TELEMETRY = "0";
      CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
      CLAUDE_CODE_IDE_SKIP_VALID_CHECK = "1";
      DISABLE_AUTOUPDATER = "1";
      DISABLE_ERROR_REPORTING = "1";
      DISABLE_INTERLEAVED_THINKING = "1";
      # DISABLE_MICROCOMPACT was here and is gone because the shipped bundle never reads it —
      # zero occurrences against a control that finds the model aliases in the same file, while
      # the microcompact machinery itself is present. Keeping it asserted that micro-compaction
      # was off when it was not, which is a worse state than not setting it. Re-check with a
      # string scan of the bundle before reinstating it.
      DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
      DISABLE_TELEMETRY = "1";
      ENABLE_EXPERIMENTAL_MCP_CLI = "false";
      ENABLE_TOOL_SEARCH = "true";
      CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
    };

    # programs.claude-code.hooks only installs the script into hooks/ and marks it
    # executable; it writes nothing to settings.json. Without this block Claude Code
    # never invokes them, which is why enforce-perl sat inert while cclens counted
    # 1,943 sed calls. Each script re-checks tool_name itself, so the Bash matcher
    # is a filter rather than the only guard.
    #
    # rtk-rewrite is installed but deliberately absent from this list. It rewrites a command to
    # run under rtk's output-compressing proxy, and rtk is not on PATH — every rewritten command
    # would exit 127, so the error and the retry would add context rather than remove it. Its
    # remaining blockers are listed in the script's own header; clear those before adding it
    # here. Registration alone would not have revealed the problem either, because an entry here
    # proves wiring and never behaviour.
    hooks.PreToolUse = [
      {
        matcher = "Bash";
        hooks = [
          {
            type = "command";
            command = "${hooksDir}/block-destructive-git";
          }
          {
            type = "command";
            command = "${hooksDir}/block-bare-cd";
          }
          {
            type = "command";
            command = "${hooksDir}/enforce-perl";
          }
        ];
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
    "define-full"
    "execute"
    "execute-full"
    "feedback"
    "markdown"
    "upstream"
  ];

  programs.claude-code.hooks.block-destructive-git = builtins.readFile "${ai-prompts-path}/hooks/block-destructive-git.sh";
  programs.claude-code.hooks.block-bare-cd = builtins.readFile "${ai-prompts-path}/hooks/block-bare-cd.sh";
  programs.claude-code.hooks.enforce-perl = builtins.readFile "${ai-prompts-path}/hooks/enforce-perl.sh";
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
      metabase-mcp.type = "local";
      metabase-mcp.command = "${nurPkgs.metabase-mcp}/bin/metabase-mcp";
    };
}
