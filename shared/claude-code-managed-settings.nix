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
  # cannot silently fail to fire here. enforce-perl is the one deliberate omission: guard-and-guide
  # carries its sed/awk rule for Claude Code, while Codex still wires the script itself.
  claudeBashHookNames = builtins.filter (n: n != "enforce-perl") shared.guardrailHookNames ++ [
    "rtk-rewrite"
  ];
in
{
  theme = "dark";
  autoUpdates = false;
  includeCoAuthoredBy = false;
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
  };

  # Claude Code fans one event out to every matching hook in parallel and merges the results
  # afterwards, so position in this list confers nothing: no hook runs before another and none
  # sees another's updatedInput. Every hook here judges the command as issued. rtk-rewrite is
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
            "rtk-rewrite"
          ];
        map (name: {
          type = "command";
          command = "${hooksDir}/${name}";
        }) claudeBashHookNames;
    }
  ];

  statusLine.type = "command";
  statusLine.command = "${aiPromptsPath}/scripts/statusline.sh";
  statusLine.padding = 0;
}
