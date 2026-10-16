#!/bin/bash
# Runs Bash commands through guard-and-guide with rules.toml and checks which ones it denies, for the
# `guard-and-guide-rules` flake check. Usage: rules.test.sh <guard-and-guide binary> <rules.toml>

set -euo pipefail

gg="$1"
rules="$2"
failures=0
checked=0

# guard-and-guide prints nothing when no rule matches.
decision() {
  local out
  out=$(jq -nc --arg c "$1" '{hook_event_name: "PreToolUse", tool_name: "Bash", tool_input: {command: $c}, session_id: "s", transcript_path: "/x", cwd: "/x"}' |
    "$gg" --config "$rules")
  if [[ -z $out ]]; then echo allow; else jq -r '.hookSpecificOutput.permissionDecision' <<<"$out"; fi
}

expect() {
  checked=$((checked + 1))
  local want="$1" cmd="$2" got
  got=$(decision "$cmd")
  [[ $got == "$want" ]] || {
    echo "FAIL: expected $want, got $got: $cmd" >&2
    failures=$((failures + 1))
  }
}

# An absolute path to a program permissions.deny names is a way past the deny pattern.
expect deny '/usr/bin/killall Finder'
expect deny '/usr/bin/sudo rm -rf x'
expect deny 'cd x && /sbin/shutdown -h now'
expect deny '"/usr/sbin/reboot"'
expect deny 'X=1 /bin/rm -rf ~'
expect deny '/usr//bin/pkill -f node'
expect deny '/usr/local/bin/dd if=/dev/zero of=x'

# Programs nothing denies by name, and paths that are data rather than an invocation.
expect allow "/bin/cat x | /usr/bin/awk '{print}'"
expect allow '/usr/bin/grep -n foo bar'
expect allow "grep '/bin/codex' notes.txt"
expect allow 'ls /usr/bin/'
expect allow '/nix/store/abc-coreutils/bin/rm x'
expect allow '/usr/bin/rmdir empty'

if ((failures > 0)); then
  echo "$failures of $checked cases failed" >&2
  exit 1
fi
echo "all $checked cases passed"
