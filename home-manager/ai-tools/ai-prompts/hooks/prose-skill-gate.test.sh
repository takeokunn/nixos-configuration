#!/bin/bash
# Table-driven test for prose-skill-gate.sh, run by the `prose-skill-gate-hook` flake check.
# Usage: prose-skill-gate.test.sh <path-to-prose-skill-gate.sh>

set -euo pipefail

hook="$1"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
main="$work/session.jsonl"
subagents="$work/session/subagents"
mkdir -p "$subagents"

skill_line() { jq -nc --arg s "$1" '{type: "assistant", message: {content: [{type: "tool_use", id: "t", name: "Skill", input: {skill: $s}}]}}'; }
edit_line() { jq -nc --arg f "$1" '{type: "assistant", message: {content: [{type: "tool_use", id: "t", name: "Edit", input: {file_path: $f, old_string: "a", new_string: "b"}}]}}'; }

# Writes the given skill names (or edit:<path> entries) to a transcript file, one line each.
transcript() {
  local file="$1"
  shift
  : >"$file"
  for item in "$@"; do
    case $item in
    edit:*) edit_line "${item#edit:}" >>"$file" ;;
    *) skill_line "$item" >>"$file" ;;
    esac
  done
}

out="$work/out"
err="$work/err"

run() {
  local status=0
  bash "$hook" <<<"$1" >"$out" 2>"$err" || status=$?
  echo "$status"
}

pre_write() { jq -nc --arg t "$main" --arg f "$1" --arg c "$2" --arg a "${3:-}" \
  '{hook_event_name: "PreToolUse", transcript_path: $t, tool_name: "Write", tool_input: {file_path: $f, content: $c}} + (if $a == "" then {} else {agent_id: $a} end)'; }
pre_bash() { jq -nc --arg t "$main" --arg c "$1" '{hook_event_name: "PreToolUse", transcript_path: $t, tool_name: "Bash", tool_input: {command: $c}}'; }
stop() { jq -nc --arg t "$main" --argjson a "${1:-false}" '{hook_event_name: "Stop", transcript_path: $t, stop_hook_active: $a}'; }

# A crash that happened to exit 2 would pass on status alone, so a block must name the missing skill.
expect_block() {
  checked=$((checked + 1))
  local name="$1" input="$2" want="$3" status
  status=$(run "$input")
  [[ $status -eq 2 ]] || fail "$name: expected exit 2, got $status ($(cat "$err"))"
  grep -q "Skill(skill: \"$want\")" "$err" || fail "$name: block did not name $want"
}

expect_allow() {
  checked=$((checked + 1))
  local name="$1" input="$2" status
  status=$(run "$input")
  [[ $status -eq 0 ]] || fail "$name: expected exit 0, got $status ($(cat "$err"))"
  [[ ! -s $out && ! -s $err ]] || fail "$name: allow produced output: $(cat "$out" "$err")"
}

expect_stop_block() {
  checked=$((checked + 1))
  local name="$1" status
  status=$(run "$(stop)")
  [[ $status -eq 0 ]] || fail "$name: expected exit 0, got $status"
  [[ $(jq -r .decision "$out" 2>/dev/null) == block ]] || fail "$name: expected a block decision, got [$(cat "$out")]"
  grep -q cold-read "$out" || fail "$name: block reason does not name cold-read"
}

transcript "$main"
expect_block "markdown without skills" "$(pre_write /repo/README.md 'Some text.')" technical-writing
expect_allow "non-prose file" "$(pre_write /repo/default.nix '{ }')"
expect_allow "agent memory store" "$(pre_write /h/.claude/projects/p/memory/note.md 'Text.')"
expect_allow "serena memory store" "$(pre_write /repo/.serena/memories/x.md 'Text.')"
expect_allow "scratch file" "$(pre_write /tmp/x/notes.md 'Text.')"

transcript "$main" technical-writing
expect_allow "english prose with technical-writing" "$(pre_write /repo/README.md 'Some text.')"
expect_block "japanese prose without yomiyasu" "$(pre_write /repo/doc.md '設定を変更する。')" yomiyasu

transcript "$main" technical-writing yomiyasu
expect_allow "japanese prose with both" "$(pre_write /repo/doc.md '設定を変更する。')"

# A similarly named skill must not satisfy the gate.
transcript "$main" technical-writing-extra
expect_block "prefix-named skill" "$(pre_write /repo/README.md 'Text.')" technical-writing

transcript "$main" technical-writing
expect_block "gh pr create without cold-read" "$(pre_bash 'gh pr create --title t --body b')" cold-read
expect_block "gh after a separator" "$(pre_bash 'cd repo && gh issue comment 1 --body b')" cold-read
expect_allow "quoted gh phrase is not a publish" "$(pre_bash 'grep -n "gh pr create" notes.txt')"
expect_allow "read-only gh" "$(pre_bash 'gh pr view 12')"

transcript "$main" technical-writing cold-read
expect_allow "gh pr create with both" "$(pre_bash 'gh pr edit 3 --body b')"

# Subagents: judged by their own transcript; a fork also counts the main one.
transcript "$main" technical-writing
transcript "$subagents/agent-sub1.jsonl"
echo '{"agentType":"docs","isFork":false}' >"$subagents/agent-sub1.meta.json"
expect_block "fresh subagent ignores main-thread load" "$(pre_write /repo/README.md 'Text.' sub1)" technical-writing
transcript "$subagents/agent-sub1.jsonl" technical-writing
expect_allow "subagent with its own load" "$(pre_write /repo/README.md 'Text.' sub1)"
transcript "$subagents/agent-fork1.jsonl"
echo '{"agentType":"fork","isFork":true}' >"$subagents/agent-fork1.meta.json"
expect_allow "fork inherits main-thread load" "$(pre_write /repo/README.md 'Text.' fork1)"

transcript "$main" technical-writing edit:/repo/README.md
expect_stop_block "stop after a prose edit"
expect_allow "stop retry is let through" "$(stop true)"

transcript "$main" technical-writing edit:/repo/README.md cold-read
expect_allow "stop after cold-read" "$(stop)"

transcript "$main" cold-read edit:/repo/guide.org
expect_stop_block "prose edited again after cold-read"

transcript "$main" edit:/repo/default.nix edit:/h/.claude/projects/p/memory/n.md
expect_allow "stop with only non-prose edits" "$(stop)"

rm "$main"
expect_allow "missing transcript" "$(pre_write /repo/README.md 'Text.')"

if ((checked == 0)); then
  echo "FAIL: no cases ran" >&2
  exit 1
fi
if ((failures > 0)); then
  echo "$failures of $checked cases failed" >&2
  exit 1
fi
echo "all $checked cases passed"
