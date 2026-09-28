#!/bin/bash
# Table-driven test for aitools-rewrite.sh, run by the `aitools-rewrite-hook` flake check.
# Usage: aitools-rewrite.test.sh <path-to-aitools-rewrite.sh>
#
# The hook only asks whether `aitools` is on PATH, so a stub stands in for it; whether the rewritten
# commands run against the real binary is outside this test.

set -euo pipefail

hook="$1"
stub_dir=$(mktemp -d)
trap 'rm -rf "$stub_dir"' EXIT
printf '#!/bin/sh\nexit 0\n' >"$stub_dir/aitools"
chmod +x "$stub_dir/aitools"
PATH="$stub_dir:$PATH"

failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

# expect_plain <command> <expected rewrite, empty for pass-through>
expect_plain() {
  checked=$((checked + 1))
  local got
  got=$(printf '%s' "$1" | bash "$hook" plain)
  [[ $got == "$2" ]] || fail "plain [$1]: expected [$2], got [$got]"
}

expect_plain 'cat flake.nix' 'aitools read flake.nix --max-lines 2000'
expect_plain 'cat -n flake.nix' 'aitools read flake.nix --max-lines 2000'
expect_plain 'cat "my file.txt"' "aitools read 'my file.txt' --max-lines 2000"
expect_plain "cat \"it's.txt\"" "aitools read 'it'\\''s.txt' --max-lines 2000"
expect_plain 'head -n 20 README.md' 'aitools read README.md --range 1:20 --max-lines 20'
expect_plain 'head -5 x.txt' 'aitools read x.txt --range 1:5 --max-lines 5'
expect_plain 'head x.txt' 'aitools read x.txt --range 1:10 --max-lines 10'
expect_plain 'tail -n 30 log.txt' 'aitools read log.txt --tail 30 --max-lines 30'
expect_plain "sed -n '10,20p' f.nix" 'aitools read f.nix --range 10:20 --max-lines 11'
expect_plain "sed -n '7p' f.nix" 'aitools read f.nix --range 7:7 --max-lines 1'
expect_plain "sed -n '5,\$p' f.nix" 'aitools read f.nix --range 5: --max-lines 2000'
expect_plain "grep -rn 'foo' src" 'aitools search foo src --context 0 --limit 100 --no-ignore'
expect_plain "grep -E 'a|b' -r ." "aitools search 'a|b' . --context 0 --limit 100 --no-ignore"
expect_plain 'grep -rl "needle" .' 'aitools search needle . --output files --limit 200 --no-ignore'
expect_plain 'grep -ric needle src' 'aitools search needle src --ignore-case --output count --limit 200 --no-ignore'
expect_plain 'grep -rn -C 2 foo src' 'aitools search foo src --context 2 --limit 100 --no-ignore'
expect_plain 'grep -rn -A3 foo src' 'aitools search foo src --context 0 --after 3 --limit 100 --no-ignore'
expect_plain 'grep -rn -e foo -e bar src' 'aitools search --pattern foo --pattern bar src --context 0 --limit 100 --no-ignore'
expect_plain "grep -rn --include='*.nix' foo ." "aitools search foo . --context 0 --limit 100 --glob '*.nix' --no-ignore"
expect_plain 'grep -Fw a.b file' 'aitools search a.b file --fixed --word --context 0 --limit 100 --no-ignore'
expect_plain "rg 'fn \\w+' -g '*.rs'" "aitools search 'fn \\w+' --context 0 --limit 100 --glob '*.rs'"
expect_plain 'rg -l TODO' 'aitools search TODO --output files --limit 200'
expect_plain 'rg -n "a.b" src --no-heading' 'aitools search a.b src --context 0 --limit 100'
expect_plain 'ls' "aitools find '*' --depth 1 --no-ignore --limit 200"
expect_plain 'ls -la home-manager' "aitools find '*' home-manager --depth 1 --no-ignore --limit 200"
expect_plain "find . -name '*.nix' -type f -maxdepth 2" "aitools find '*.nix' . --type file --depth 2 --no-ignore --limit 200"
expect_plain 'find src -type d' "aitools find '*' src --type dir --no-ignore --limit 200"
expect_plain 'diff a.txt b.txt' 'aitools diff a.txt b.txt'
expect_plain 'git status' 'aitools git status'
expect_plain 'git status -sb' 'aitools git status'
expect_plain 'git log --oneline -5' 'aitools git log --limit 5'
expect_plain 'git log -n 3 -- flake.nix' 'aitools git log flake.nix --limit 3'
expect_plain 'git diff' 'aitools git diff'
expect_plain 'git diff --cached --stat' 'aitools git diff --staged --output stat'
expect_plain 'git diff -- flake.nix' 'aitools git diff flake.nix'
expect_plain 'git show HEAD:flake.nix' 'aitools git show HEAD:flake.nix --max-lines 2000'
expect_plain 'git blame -L 10,20 flake.nix' 'aitools git blame flake.nix --range 10:20'
expect_plain '  cat flake.nix  ' 'aitools read flake.nix --max-lines 2000'

# Pass-through: anything the rewrite could change the meaning of.
for cmd in \
  'cat a b' \
  'cat' \
  'cat -- -weird' \
  'head -n 0 x' \
  'tail -f log.txt' \
  'tail -n +5 log.txt' \
  "sed -i 's/a/b/' f.nix" \
  "sed -n '20,10p' f.nix" \
  "sed -n 's/a/b/p' f.nix" \
  'wc -l f.nix' \
  'git status --porcelain' \
  'git log --foo' \
  'git blame a b' \
  'rg -e --no-mask file.txt' \
  'grep -r -e --output=raw .' \
  'grep -r -- -foo .' \
  'rg -g --limit=1 pattern .' \
  'grep -r --include=-x foo .' \
  "find . -name '-*'" \
  'grep foo' \
  'grep -v foo x' \
  "grep -rn 'foo\\|bar' src" \
  "grep -rn 'a+' src" \
  'grep -rlc foo src' \
  "grep -E '\\<word' -r ." \
  'rg -uu x' \
  "rg '\\p{Greek}' src" \
  'ls *.nix' \
  'ls -R' \
  'ls a b' \
  'find . -name foo' \
  'find . -name "*.nix" -delete' \
  'find . -exec rm {} ;' \
  'find . -iname "*.nix"' \
  'diff -r a b' \
  'git diff HEAD~1' \
  'git log --all' \
  'git push' \
  'git stash' \
  'git show HEAD' \
  'cat $HOME/.x' \
  'cat "$HOME/.x"' \
  'cat ~/.x' \
  'cat file | head' \
  'cat a && cat b' \
  'cat a; cat b' \
  'cat a > b' \
  'cat <(ls)' \
  'cat `ls`' \
  'FOO=1 cat x' \
  'command cat x' \
  'cat "unterminated' \
  "cat it\\'s.txt" \
  'cat a # comment' \
  'bat x' \
  ''; do
  expect_plain "$cmd" ''
done
expect_plain $'cat a\ncat b' ''

# expect_json <mode> <payload> <jq filter that must print true>
expect_json() {
  checked=$((checked + 1))
  local got
  got=$(printf '%s' "$2" | bash "$hook" "$1")
  if [[ -z $3 ]]; then
    [[ -z $got ]] || fail "$1 [$2]: expected no output, got [$got]"
    return
  fi
  [[ $(jq -r "$3" <<<"$got" 2>/dev/null) == true ]] || fail "$1 [$2]: [$3] not true for [$got]"
}

payload='{"tool_name":"Bash","hook_event_name":"PreToolUse","tool_input":{"command":"cat flake.nix","description":"read","timeout":5000}}'
expect_json claude "$payload" '.hookSpecificOutput.updatedInput.command == "aitools read flake.nix --max-lines 2000"'
expect_json claude "$payload" '.hookSpecificOutput.hookEventName == "PreToolUse"'
expect_json claude "$payload" '.hookSpecificOutput | has("permissionDecision") | not'
expect_json claude "$payload" '.hookSpecificOutput.updatedInput.description == "read" and .hookSpecificOutput.updatedInput.timeout == 5000'
expect_json codex "$payload" '.hookSpecificOutput.permissionDecision == "allow"'
expect_json codex "$payload" '.hookSpecificOutput.updatedInput.command == "aitools read flake.nix --max-lines 2000"'
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"cat flake.nix"}}' '.hookSpecificOutput.updatedInput.command | startswith("aitools read")'
expect_json claude '{"tool_name":"Bash","hook_event_name":"PostToolUse","tool_input":{"command":"cat flake.nix"}}' ''
expect_json claude '{"tool_name":"Read","tool_input":{"file_path":"flake.nix"}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"git push"}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":{}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":"cat x"}' ''
expect_json claude 'not json' ''
expect_json claude '' ''
expect_json codex '{"tool_name":"Bash","tool_input":{"command":"git push"}}' ''

# Without aitools on PATH the hook must stand aside rather than hand back a command that cannot run.
checked=$((checked + 1))
bare_dir="$stub_dir/bare"
mkdir "$bare_dir"
ln -s "$(command -v perl)" "$bare_dir/perl"
ln -s "$(command -v cat)" "$bare_dir/cat"
got=$(printf '%s' 'cat flake.nix' | PATH="$bare_dir" "$BASH" "$hook" plain)
[[ -z $got ]] || fail "no aitools on PATH: expected no output, got [$got]"

echo "aitools-rewrite: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
