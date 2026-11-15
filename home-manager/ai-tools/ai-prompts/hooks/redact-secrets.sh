#!/bin/bash
# PostToolUse hook that replaces GitHub-token-shaped strings in a tool's output with
# `[REDACTED:<rule>]` before the model sees them. Every string anywhere inside `.tool_response`
# (object, array, or bare string) is scanned; the rules are in `rules` below.
#
# The first argument selects the output shape:
#   claude (default)  hookSpecificOutput.updatedToolOutput (updatedMCPToolOutput for an `mcp__` tool)
#                     holding tool_response with only string values changed. Claude Code validates a
#                     built-in tool's replacement against that tool's output schema and falls back to
#                     the original on mismatch, so the shape must stay identical.
#   codex             {"decision":"block","reason":<redacted body>}: Codex hands the model only the
#                     reason. The body is tool_response itself when it is a string, else compact JSON.
#
# No match prints nothing. Any failure of the hook itself (unreadable JSON, jq missing) also prints
# nothing and exits 0: stdout is recorded in the transcript and stderr may be shown, so neither may
# carry the input, and an exit status other than 0 does not stop the output in either agent.
#
# Rule notes:
#   github-token and github-pat follow GitHub's documented prefixes and lengths.
#   nix-access-tokens takes the rest of the line, since nix.conf's access-tokens is a space-separated
#     list of host=token pairs and `\S+` would stop after the first.
#   credential-password matches only at a line start, the form `git credential fill` and credential
#     helpers print (`password=<value>`), so prose such as "set password=..." mid-line is left alone.
#   authorization-header needs a scheme and a value, so a bare `Authorization: Bearer` is left alone.

set -uo pipefail

mode="${1:-claude}"
case "$mode" in
claude | codex) ;;
*) exit 0 ;;
esac

out=$(jq -c --arg mode "$mode" '
  def rules: [
    {name: "github-pat", re: "github_pat_[A-Za-z0-9_]{60,}", flags: "g"},
    {name: "github-token", re: "gh[pousr]_[A-Za-z0-9]{36,}", flags: "g"},
    {name: "nix-access-tokens", re: "access-tokens *= *\\S[^\\r\\n]*", flags: "g"},
    {name: "credential-password", re: "(?m)^password=[^\\r\\n]+", flags: "g"},
    {name: "authorization-header", re: "Authorization: *(token|bearer|basic) +\\S+", flags: "gi"}
  ];
  def redact: reduce rules[] as $r (.; gsub($r.re; "[REDACTED:\($r.name)]"; $r.flags));

  .tool_response as $orig
  | ($orig | (.. | strings) |= redact) as $red
  | if $red == $orig then empty
    elif $mode == "codex" then
      {decision: "block", reason: (if ($red | type) == "string" then $red else ($red | tojson) end)}
    else
      {hookSpecificOutput: (
        {hookEventName: "PostToolUse"}
        + if ((.tool_name | strings) // "" | startswith("mcp__"))
          then {updatedMCPToolOutput: $red}
          else {updatedToolOutput: $red} end)}
    end
' 2>/dev/null) || exit 0

[[ -n $out ]] && printf '%s\n' "$out"
exit 0
