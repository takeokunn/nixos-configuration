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

  shared = import ../shared { inherit (pkgs) lib; };

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
    enableAllProjectMcpServers = true;
    feedbackSurveyState.lastShownTime = 1754089004345;
    outputStyle = "Explanatory";

    permissions = {
      # FR-008: the raw pattern list is shared/default.nix's bashDenyPatterns (the union of
      # this list and opencode's own, see that file). Each pattern is wrapped in Claude Code's
      # own "Bash(<p>)" permission-rule syntax here.
      deny = map (p: "Bash(${p})") shared.bashDenyPatterns;
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
        # FR-008: shared/default.nix's guardrailHookNames is the single source of truth for
        # this roster (codex/default.nix registers the same list, generated with `map`). The
        # entries here stay spelled out, so the assert below is the whole guard against them
        # drifting from the shared list -- it fails the build rather than shipping a hook that
        # is installed and never fires, which is the failure mode this roster exists to prevent
        # and which went unnoticed once before.
        hooks =
          assert
            shared.guardrailHookNames == [
              "block-destructive-git"
              "block-bare-cd"
              "enforce-perl"
            ];
          [
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
    // shared.mcpServers { inherit nurPkgs; };
}
