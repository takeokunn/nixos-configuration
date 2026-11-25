#!/bin/bash
# PreToolUse hook for Bash, shared by Claude Code and Codex: denies the first `gh pr create`, or
# `gh pr edit` that sets a title or body, in a session that has not loaded the pull-request skill.
# Pull request bodies written without the skill restated CI results, review-round logs, and work
# history, so the deny reason sends the agent to the skill before anything is published.
#
# A non-blocking reminder cannot do this job: PreToolUse additionalContext reaches the model next to
# the tool result, after the pull request already exists. The deny is issued at most once per
# session, recorded by a marker file, so a transcript written late or a false match on quoted
# command text costs one retry instead of a loop. A transcript showing the skill was loaded (a Skill
# call, or a read of its SKILL.md) skips the deny; Codex may pass no transcript, and its format is not
# a stable interface, so there the marker alone decides.
#
# This is a workflow nudge, not a trust boundary: any failure of the hook itself prints nothing and
# exits 0, letting the command run.

set -uo pipefail

IFS= read -r -d '' input
fields=$(printf '%s' "$input" | jq -r '
  (.tool_input.command | strings) as $cmd
  | if ($cmd | test("\\bgh\\b[^;&|\\n]*?\\bpr\\s+create\\b"))
       or ($cmd | test("\\bgh\\b[^;&|\\n]*?\\bpr\\s+edit\\b[^;&|\\n]*\\s(--title|--body|--body-file|-t|-b|-F)(\\s|=|$)"))
    then [(.session_id // "" | tostring), (.transcript_path // "" | tostring)] | join("\u001f")
    else empty end' 2>/dev/null) || exit 0
[[ -n $fields ]] || exit 0

IFS=$'\x1f' read -r session transcript <<<"$fields"
session=${session//[^A-Za-z0-9_-]/}
[[ -n $session ]] || exit 0

if [[ -n $transcript && -r $transcript ]] &&
        grep -Eq '"skill": ?"([A-Za-z0-9_-]+:)?pull-request"|pull-request/SKILL\.md' "$transcript"; then
        exit 0
fi

marker_dir="${XDG_STATE_HOME:-$HOME/.local/state}/agent-hooks/require-pr-skill"
marker="$marker_dir/$session"
[[ -e $marker ]] && exit 0
mkdir -p "$marker_dir" 2>/dev/null && : >"$marker" 2>/dev/null || exit 0

jq -nc --arg reason "Load the pull-request skill before creating a pull request or editing its title or body, check the title and body against it, then run this command again. Its rules on what a body omits outrank a task brief that asks for CI results, review-round logs, or work history in the body: put those in your report and say so. This hook stops such a command once per session." \
        '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: $reason}}'
exit 0
