# Claude Code's managed-settings policy, shared by the darwin and NixOS system modules.
#
# Policy this repository enforces (permissions, hooks, env) lives here, in the managed-settings
# file, which is read at higher precedence than user settings and than the command line. User
# preference defaults (model, effort) are merged into the writable `~/.claude/settings.json` by
# home-manager's `programs.claude-code.mutableSettings` instead, and re-applied on each activation.
#
# This is the one file reaching from a system module into `home-manager/ai-tools`: the guardrail
# catalog and the hook scripts are authored there, and a second copy here would drift. The
# `../shared` module is a pure `{ lib }:` function with no home-manager coupling, so importing it
# pulls in no module-system state.
{
  lib,
  pkgs,
  guardAndGuide,
  # Absolute home directory of the governed user. System-level policy cannot see home-manager's
  # `programs.claude-code.configDir`, so the hook paths built from this must match it by hand.
  homeDirectory,
}:
let
  aiPromptsPath = ../home-manager/ai-tools/ai-prompts;

  shared = import ../home-manager/ai-tools/shared { inherit lib; };

  hooksDir = "${homeDirectory}/.claude/hooks";

  # Installed by home-manager/ai-tools/gbrain, which asserts this path.
  gbrainHook = "${homeDirectory}/.local/share/gbrain/hook";
  gbrainCaptureHook = subcommand: timeout: [
    {
      hooks = [
        {
          type = "command";
          command = "${gbrainHook} ${subcommand}";
          inherit timeout;
        }
      ];
    }
  ];

  guardAndGuideHook = pkgs.writeShellScript "guard-and-guide" ''
    exec ${guardAndGuide}/bin/guard-and-guide --config ${aiPromptsPath}/hooks/rules.toml
  '';

  # Claude Code prefixes every Bash command with the contents of CLAUDE_ENV_FILE, which it sets only
  # for SessionStart hooks. The unsets repeat the claude wrapper's for launches that bypass it.
  scrubEnvHook = pkgs.writeShellScript "scrub-agent-env" ''
    if [ -n "''${CLAUDE_ENV_FILE:-}" ]; then
      printf '%s\n' ${lib.escapeShellArg "unset ${lib.concatStringsSep " " shared.agentScrubbedEnvNames}"} >>"$CLAUDE_ENV_FILE"
    fi
  '';

  redactHook = pkgs.writeShellScript "redact-secrets" ''
    export PATH=${pkgs.jq}/bin:$PATH
    exec ${pkgs.bash}/bin/bash ${aiPromptsPath}/hooks/redact-secrets.sh claude
  '';

  # Derived from the shared catalog rather than restated, so a guardrail added there for Codex
  # cannot silently fail to fire here.
  claudeBashHookNames = shared.guardrailHookNames ++ [
    "aitools-rewrite"
    "require-pr-skill"
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
  enableAllProjectMcpServers = false;
  # Selects the style defined in the home-manager module. Built-in Explanatory cannot be extended,
  # only replaced, so explanatory-strict re-authors its insight behavior and adds
  # output_discipline's prohibitions at the system-prompt layer, which CLAUDE.md cannot reach. The style
  # restates those prohibitions rather than referencing them, so keep the two aligned when either changes.
  outputStyle = "explanatory-strict";
  # Injects "Always respond in <language>" at the system-prompt layer, which CLAUDE.md's Japanese-reply
  # rule cannot reach; it also sets the voice-dictation language. Nothing here rejects an unknown key,
  # so re-check the key and value against the settings schema after a CLI bump.
  language = "japanese";

  permissions = {
    deny = map (p: "Bash(${p})") shared.bashDenyPatterns ++ [
      # Slite comes from programs.claude-code.mcpServers, which home-manager serves as the "hm" plugin,
      # so its tools are named mcp__plugin_hm_slite__*.
      "mcp__plugin_hm_slite__archive-note"
      "mcp__plugin_hm_slite__remove-blocks"
      # ClickUp is a local-scope server registered as "clickup"; the name must match these rules (see
      # copy-local-mcp-servers.sh).
      "mcp__clickup__clickup_delete_task"
      "mcp__clickup__clickup_delete_comment"
      "mcp__clickup__clickup_delete_chat_message"
      "mcp__clickup__clickup_merge_tasks"
    ];
    # bypassPermissions skips every prompt and the auto-mode classifier; deny rules and the PreToolUse
    # hooks still apply. Ask rules would prompt, including for an already authorized override.
    # Headless `claude -p` refuses MCP tools that no allow rule names.
    allow = [
      "mcp__plugin_hm_gbrain"
      "mcp__plugin_hm_ast-grep"
    ];
    ask = [ ];
    defaultMode = "bypassPermissions";
  };
  # Accepts the one-time bypass warning dialog up front; background sessions honor it only from user or
  # managed settings.
  skipDangerousModePermissionPrompt = true;

  env = {
    BASH_DEFAULT_TIMEOUT_MS = "300000";
    BASH_MAX_TIMEOUT_MS = "1200000";
    CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
    MCP_TOOL_TIMEOUT = "120000";
    CLAUDE_CODE_MAX_OUTPUT_TOKENS = "32000";
    CLAUDE_CODE_AUTO_CONNECT_IDE = "0";
    # Also covers telemetry, error reporting, and the autoupdater (code.claude.com/docs/en/env-vars).
    CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
    CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
    CLAUDE_CODE_IDE_SKIP_VALID_CHECK = "1";
    CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
    # Trusts the nixpkgs cacert bundle in /etc/ssl. It guards against CLAUDE_CODE_CERT_STORE=system,
    # which on macOS fails API calls with UNABLE_TO_GET_ISSUER_CERT_LOCALLY; whether it is still
    # needed is unmeasured, and on NixOS it duplicates the system store.
    # CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST (set by the Mediator desktop app) makes Claude Code ignore
    # this entry, so home-manager/ai-tools/claude-code/default.nix also sets it in the wrapper; this
    # entry covers launches that bypass that wrapper.
    NODE_EXTRA_CA_CERTS = shared.claudeCodeCaBundle;
    # Memory lives in gbrain (home-manager/ai-tools/gbrain); auto memory would be a second store.
    CLAUDE_CODE_DISABLE_AUTO_MEMORY = "1";
  };

  # Claude Code runs every matching hook in parallel and merges the results, so list order confers
  # nothing and each hook judges the command as issued. aitools-rewrite is the only hook emitting
  # updatedInput; a second one would make precedence non-deterministic.
  #
  # Hook lists merge across settings sources, so entries a tool installs into the user's own
  # settings.json still fire alongside these.
  hooks.PreToolUse = [
    # Only the empty matcher reaches Read, Write, and Edit; the Bash entry below cannot see them.
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${guardAndGuideHook}";
          onFailure = "block";
        }
      ];
    }
    {
      matcher = "Bash";
      # Without the assert, a guardrail renamed or added in the shared catalog would be installed
      # and never fire.
      hooks =
        assert
          claudeBashHookNames == [
            "block-destructive-git"
            "aitools-rewrite"
            "require-pr-skill"
          ];
        map (
          name:
          {
            type = "command";
            # `claude` scans a direct jj push before allowing it without a permission prompt.
            command = "${hooksDir}/${name}" + lib.optionalString (name == "block-destructive-git") " claude";
          }
          // lib.optionalAttrs (name == "block-destructive-git") {
            onFailure = "block";
          }
        ) claudeBashHookNames;
    }
  ];

  # The last layer against a token reaching the model: it rewrites only what a tool returns, and it
  # is fail-open by design (a hook failure leaves the output as it was). A failed tool call does not
  # run PostToolUse, so its error output is not covered.
  hooks.PostToolUse = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${redactHook}";
        }
      ];
    }
  ];

  # The empty matcher covers every session source (startup, resume, clear, compact, fork).
  hooks.SessionStart = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${scrubEnvHook}";
        }
        # gbrain waits up to 4 s for its server inside this budget.
        {
          type = "command";
          command = "${gbrainHook} session-start --harness claude-code";
          timeout = 5;
        }
      ];
    }
  ];

  hooks.UserPromptSubmit = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${gbrainHook} user-prompt --harness claude-code";
          timeout = 3;
        }
      ];
    }
  ];

  # gbrain's capture lanes: Stop banks the turn, PreCompact banks the window's entities before
  # compaction, SessionEnd hands the transcript to the serve for extraction. Commands and timeouts
  # follow `gbrain setup claude-code --capture` and gbrain bootstrap's PreCompact entry. gbrain
  # discards Stop-banked turns unless memory.auto_writeback is set (home-manager/ai-tools/gbrain).
  hooks.Stop = gbrainCaptureHook "stop" 10;
  hooks.PreCompact = gbrainCaptureHook "compact" 5;
  hooks.SessionEnd = gbrainCaptureHook "session-end" 60;

  statusLine.type = "command";
  statusLine.command = "${aiPromptsPath}/scripts/statusline.sh";
  statusLine.padding = 0;
}
