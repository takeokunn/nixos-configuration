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
  # Commit and PR text comes from the pull-request skill, so Claude Code adds no trailer of its own.
  attribution = {
    commit = "";
    pr = "";
  };
  # The built-in commit and PR instructions conflict with CLAUDE.md's hard_rules (they branch from the
  # default branch unasked); the pull-request and jujutsu skills carry this workflow instead.
  includeGitInstructions = false;
  autoCompactEnabled = true;
  enableAllProjectMcpServers = true;
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
    # bypassPermissions skips every prompt and the auto-mode classifier; deny rules and the PreToolUse
    # hooks still apply. Allow rules have no effect in this mode and an ask rule would still prompt, so
    # the only ask rules left are the override prefixes that lift a block-destructive-git verdict.
    ask = [
      "Bash(ALLOW_DESTRUCTIVE_GIT=1 *)"
      "Bash(CLAUDE_ALLOW_DESTRUCTIVE_GIT=1 *)"
    ];
    defaultMode = "bypassPermissions";
  };
  # Accepts the one-time bypass warning dialog up front; background sessions honor it only from user or
  # managed settings.
  skipDangerousModePermissionPrompt = true;

  # "$defaults" keeps the built-in environment; the entries after it name where this user's work lives.
  autoMode.environment = [
    "$defaults"
    "Source control: repositories under the user's own owner directories in the ghq root (the user's account and the organizations the user belongs to) and those repositories' configured remotes. Third-party clones under the ghq root are reference copies, not trusted destinations."
    "jj workspaces and .worktrees directories of those repositories are part of the same trusted repository."
    "Scratch: /tmp, /private/tmp, and the system temp directory are ephemeral agent scratch space."
    "Contest servers: only SSH host aliases whose names start with \"isu\" in the user's ssh config, used for ISUCON practice and contests; benchmarking, editing configs, restarting services, and reading logs there is routine."
    "Deleting, moving, and rebasing files and jj changes inside .worktrees checkouts and jj workspaces that this session created is routine; other sessions' checkouts and workspaces are not."
    "Stopping background processes the agent itself started in this session, by PID, is routine."
    "The Mediator MCP server is the user's own agent orchestrator; sending messages, plans, and turns through it is routine delegation unless the message directs a push, merge, publication, or deletion."
  ];

  env = {
    BASH_DEFAULT_TIMEOUT_MS = "300000";
    BASH_MAX_TIMEOUT_MS = "1200000";
    CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
    MCP_TOOL_TIMEOUT = "120000";
    CLAUDE_CODE_MAX_OUTPUT_TOKENS = "32000";
    CLAUDE_CODE_SUBAGENT_MODEL = "opus";
    CLAUDE_CODE_AUTO_CONNECT_IDE = "0";
    # Also covers telemetry, error reporting, and the autoupdater (code.claude.com/docs/en/env-vars).
    CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
    CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
    CLAUDE_CODE_IDE_SKIP_VALID_CHECK = "1";
    CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
    # Adds the cacert bundle that nix-darwin places in /etc/ssl to the trusted roots, so they follow
    # nixpkgs rather than the binary. The llm-agents claude wrapper used to set
    # CLAUDE_CODE_CERT_STORE=system, which on macOS left only admin-trusted certificates and failed API
    # calls with UNABLE_TO_GET_ISSUER_CERT_LOCALLY; it no longer does, and whether this entry is still
    # needed is unmeasured. On NixOS it is expected to duplicate the system store.
    #
    # A launcher that sets CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST (the Mediator desktop app does)
    # makes Claude Code ignore this entry, so the claude wrapper in
    # home-manager/ai-tools/claude-code/default.nix also sets the bundle in the process environment.
    # This entry stays for launches that bypass that wrapper.
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
            "aitools-rewrite"
          ];
        map (name: {
          type = "command";
          # `claude` makes the hook answer a jj push with an ask decision instead of exit 2, so the
          # person confirms it; Codex runs the same script with no argument and keeps the block.
          command = "${hooksDir}/${name}" + lib.optionalString (name == "block-destructive-git") " claude";
        }) claudeBashHookNames;
    }
  ];

  statusLine.type = "command";
  statusLine.command = "${aiPromptsPath}/scripts/statusline.sh";
  statusLine.padding = 0;
}
