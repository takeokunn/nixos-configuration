# Claude Code's managed-settings policy, shared by the darwin and NixOS system modules.
#
# Claude Code expects `~/.claude/settings.json` to be writable: it persists a `/model` choice
# there, and tools that install hook entries resolve the path and write to it. Placing that file
# from `home.file` makes it a read-only store symlink, so both writes fail silently. Everything
# this repository wants to pin therefore lives here instead, in the managed-settings file, which
# is read at higher precedence than user settings and than the command line.
#
# This is the one file reaching from a system module into `home-manager/ai-tools`: the guardrail
# catalog and the hook scripts are authored there, and a second copy here would drift. The
# `../shared` module is a pure `{ lib }:` function with no home-manager coupling, so importing it
# pulls in no module-system state.
{
  lib,
  guardAndGuide,
  # Absolute home directory of the user these settings govern. The hook commands need it because
  # this file is evaluated as system-level policy, where home-manager's own `programs.claude-code.configDir`
  # is out of scope. flake.nix asserts the two agree.
  homeDirectory,
}:
let
  aiPromptsPath = ../home-manager/ai-tools/ai-prompts;

  shared = import ../home-manager/ai-tools/shared { inherit lib; };

  hooksDir = "${homeDirectory}/.claude/hooks";

  # Derived from the shared catalog rather than restated, so a guardrail added there for Codex
  # cannot silently fail to fire here.
  claudeBashHookNames = shared.guardrailHookNames ++ [
    "aitools-rewrite"
  ];
in
{
  theme = "dark";
  autoUpdates = false;
  includeCoAuthoredBy = false;
  # The built-in commit and PR instructions conflict with CLAUDE.md's hard_rules (they branch from the
  # default branch unasked); the pull-request and jujutsu skills carry this workflow instead.
  includeGitInstructions = false;
  autoCompactEnabled = true;
  enableAllProjectMcpServers = true;
  feedbackSurveyState.lastShownTime = 1754089004345;
  # Selects the style defined in the home-manager module. Built-in Explanatory cannot be extended,
  # only replaced, so explanatory-strict re-authors its insight behavior and adds
  # output_discipline's prohibitions at the system-prompt layer, which CLAUDE.md cannot reach. The style
  # restates those prohibitions rather than referencing them, so keep the two aligned when either changes.
  outputStyle = "explanatory-strict";
  # Injects "Always respond in <language>" at the system-prompt layer, which CLAUDE.md's Japanese-reply
  # rule cannot reach; it also sets the voice-dictation language. Key and value checked against the
  # settings schema in Claude Code 2.1.281. Nothing here rejects an unknown key, so re-check after a
  # CLI bump.
  language = "japanese";

  permissions = {
    deny = map (p: "Bash(${p})") shared.bashDenyPatterns;
    # An allow rule skips the auto-mode classifier, so Bash(*) leaves the deny and ask lists and the
    # PreToolUse hooks as the only guards on shell commands. block-destructive-git still blocks the jj
    # commands that move or discard a shared working copy and every unscanned `jj git push`, but it
    # exits 0 when neither jq nor python3 is on PATH, which leaves jj with no check at all.
    # Claude Code may drop a blanket Bash rule when auto mode starts (unverified for this version): if
    # a command that is not denied still prompts after a rebuild, replace Bash(*) with per-command
    # entries for gh, jj and the read-only nix subcommands.
    # slite and clickup stay prompted because their writes publish to external workspaces.
    allow = [
      "Bash(*)"
      "mcp__mediator"
      "mcp__plugin_hm_context7"
      "mcp__plugin_hm_deepwiki"
      "mcp__plugin_hm_playwright"
      "mcp__plugin_hm_serena"
    ];
    # Ask outranks allow, so these forms of an allowed command still prompt. Each can start a program
    # the allow rule never named: jj through a configured tool, editor, pager, or fixer; nix through an
    # impure read of any file. The override prefix lifts block-destructive-git's jj verdicts, and an
    # agent can type it, so a person confirms each use.
    ask = [
      "Bash(jj *--config*)"
      "Bash(jj *--tool*)"
      "Bash(jj config set *)"
      "Bash(jj config edit*)"
      "Bash(jj fix*)"
      "Bash(jj run *)"
      "Bash(jj sign*)"
      "Bash(jj gerrit *)"
      "Bash(nix eval *--impure*)"
      "Bash(ALLOW_DESTRUCTIVE_GIT=1 *)"
      "Bash(CLAUDE_ALLOW_DESTRUCTIVE_GIT=1 *)"
    ];
    # Reviewed via /define: hooks and permissions.deny still enforce independently of
    # permission mode, so this only removes the interactive prompt; a background classifier
    # model now reviews everything else auto mode would have prompted for (see Claude Code
    # docs: "Eliminate permission prompts with auto mode").
    defaultMode = "auto";
    # Neutralises `--dangerously-skip-permissions` on launches this machine does not control:
    # tools that spawn unattended agents hardcode that flag, which would otherwise outrank
    # `defaultMode` and disable every check above. Measured against Claude Code 2.1.278: the
    # process still starts and exits 0, the flag is ignored, and the session falls back to
    # `defaultMode`, so an unattended agent degrades rather than dies. The version matters —
    # anthropics/claude-code#44642 reports this key having no effect at all on 2.1.92. Re-measure
    # after a major CLI bump rather than trusting this line.
    disableBypassPermissionsMode = "disable";
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
    # The claude package from the llm-agents input sets CLAUDE_CODE_CERT_STORE=system in its
    # wrapper, so that nixpkgs' cacert decides the trusted roots. On macOS that store holds only
    # admin-trusted certificates, not Apple's built-in roots, and API calls fail with
    # UNABLE_TO_GET_ISSUER_CERT_LOCALLY. Adding the cacert bundle that nix-darwin places in /etc/ssl
    # restores the roots and keeps them following nixpkgs; switching back to `bundled,system` would
    # pin them to the binary instead. On NixOS the entry is expected to duplicate the system store.
    #
    # A host that launches claude with CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST set (Mediator does)
    # makes Claude Code ignore this entry, so the claude wrapper in
    # home-manager/ai-tools/claude-code/default.nix also sets the bundle in the process environment.
    # This entry stays for launches that bypass that wrapper. Drop both once `claude --debug`, run
    # with neither in effect, logs a "Loaded N system CA certificates" count that includes Apple's
    # roots (over 100) rather than the admin-trusted few.
    NODE_EXTRA_CA_CERTS = shared.claudeCodeCaBundle;
  };

  # Claude Code fans one event out to every matching hook in parallel and merges the results
  # afterwards, so position in this list confers nothing: no hook runs before another and none
  # sees another's updatedInput. Every hook here judges the command as issued. aitools-rewrite is
  # currently the only one emitting updatedInput; adding a second would make precedence between
  # them non-deterministic, and this list could not resolve it.
  #
  # Hook lists merge across settings sources, so entries a tool installs into the user's own
  # settings.json still fire alongside these.
  hooks.PreToolUse = [
    # The empty matcher is what reaches Read, Write, and Edit; the Bash entry below cannot see
    # them, which is the gap guard-and-guide was added to close.
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${guardAndGuide}/bin/guard-and-guide --config ${aiPromptsPath}/hooks/rules.toml";
        }
      ];
    }
    {
      matcher = "Bash";
      # The assert fails the build if the derived wiring stops matching what this file expects:
      # a guardrail renamed or added in the shared catalog would otherwise be installed and never
      # fire, which has gone unnoticed here once before.
      hooks =
        assert
          claudeBashHookNames == [
            "block-destructive-git"
            "block-bare-cd"
            "aitools-rewrite"
          ];
        map (name: {
          type = "command";
          command = "${hooksDir}/${name}";
        }) claudeBashHookNames;
    }
    # Claude Code only: it judges skill loads from the session transcript, whose format Codex does not
    # share. Bash is matched for the `gh pr|issue` publish commands.
    {
      matcher = "Write|Edit|MultiEdit|Bash";
      hooks = [
        {
          type = "command";
          command = "${hooksDir}/prose-skill-gate";
        }
      ];
    }
  ];

  hooks.Stop = [
    {
      hooks = [
        {
          type = "command";
          command = "${hooksDir}/prose-skill-gate";
        }
      ];
    }
  ];

  statusLine.type = "command";
  statusLine.command = "${aiPromptsPath}/scripts/statusline.sh";
  statusLine.padding = 0;
}
