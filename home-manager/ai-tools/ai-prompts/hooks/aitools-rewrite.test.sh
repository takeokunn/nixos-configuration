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

# The globs rg, fd, and tree rewrites add so that dot entries stay hidden, as they are in those tools.
hidden="--glob '!.*' --glob '!**/.*/**'"

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
# rg -t also lists the dot-files of its type, so it gets only the dot-directory glob.
dot_dirs="--glob '!**/.*/**'"
expect_plain "rg -l foo -g '!*.min.js'" "aitools search foo --output files --limit 200 --glob '!*.min.js' $hidden"
expect_plain "rg -l foo -t nix -g '!*.md'" "aitools search foo --output files --limit 200 --glob '!*.md' --lang nix $dot_dirs"
expect_plain 'rg -l TODO' "aitools search TODO --output files --limit 200 $hidden"
expect_plain 'rg -n "a.b" src --no-heading' "aitools search a.b src --context 0 --limit 100 $hidden"
expect_plain 'rg -l mkIf .' "aitools search mkIf . --output files --limit 200 $hidden"
expect_plain 'rg -l mkIf ./src' "aitools search mkIf ./src --output files --limit 200 $hidden"
expect_plain 'rg -o "v[0-9]+" f' "aitools search 'v[0-9]+' f --output matches --limit 100 $hidden"
expect_plain 'rg --only-matching foo' "aitools search foo --output matches --limit 100 $hidden"
expect_plain 'rg -v foo f' "aitools search foo f --invert --context 0 --limit 100 $hidden"
expect_plain 'rg -x foo f' "aitools search foo f --line-regexp --context 0 --limit 100 $hidden"
expect_plain 'rg -U "a.b" src' "aitools search a.b src --multiline --context 0 --limit 100 $hidden"
expect_plain 'rg -t nix mkIf' "aitools search mkIf --context 0 --limit 100 --lang nix $dot_dirs"
expect_plain "rg 'a{2,3}b\\{' f" "aitools search 'a{2,3}b\\{' f --context 0 --limit 100 $hidden"
expect_plain 'rg -tpy import' "aitools search import --context 0 --limit 100 --lang python $dot_dirs"
expect_plain 'rg --type ts foo' "aitools search foo --context 0 --limit 100 --lang typescript $dot_dirs"
expect_plain 'rg -il foo -t rust' "aitools search foo --ignore-case --output files --limit 200 --lang rust $dot_dirs"
expect_plain 'rg --files' "aitools find '*' --type file $hidden --limit 200"
expect_plain 'rg --files src' "aitools find '*' src --type file $hidden --limit 200"
expect_plain "rg --files -g '!*.lock'" "aitools find '*' --type file --glob '!*.lock' $hidden --limit 200"
expect_plain 'rg --files -t go src' "aitools find '*' src --type file --lang go $dot_dirs --limit 200"
expect_plain 'grep -o foo f' 'aitools search foo f --output matches --limit 100 --no-ignore'
expect_plain 'grep -v foo x' 'aitools search foo x --invert --context 0 --limit 100 --no-ignore'
expect_plain 'grep -rnx foo src' 'aitools search foo src --line-regexp --context 0 --limit 100 --no-ignore'
expect_plain 'grep --invert-match -r foo .' 'aitools search foo . --invert --context 0 --limit 100 --no-ignore'
expect_plain 'fd' "aitools find '*' $hidden --limit 200"
expect_plain 'fd .' "aitools find '*' $hidden --limit 200"
expect_plain 'fd README' "aitools find README $hidden --limit 200"
expect_plain 'fd -t f Config src' "aitools find Config src --type file $hidden --limit 200"
expect_plain 'fd -td . home' "aitools find '*' home --type dir $hidden --limit 200"
expect_plain 'fd --type=file -d 2 X' "aitools find X --type file --depth 2 $hidden --limit 200"
expect_plain 'fd -H Makefile' "aitools find Makefile --limit 200"
expect_plain 'fd -H Makefile .config' "aitools find Makefile .config --limit 200"
expect_plain 'fd -I Cargo' "aitools find Cargo --no-ignore $hidden --limit 200"
expect_plain 'tree' "aitools find '*' --output tree --no-ignore $hidden --limit 200"
expect_plain 'tree -L 2 src' "aitools find '*' src --output tree --depth 2 --no-ignore $hidden --limit 200"
expect_plain 'tree -d' "aitools find '*' --output tree --type dir --no-ignore $hidden --limit 200"
expect_plain 'wc -c flake.lock' 'aitools info flake.lock'
expect_plain 'jq . flake.lock' "aitools json get flake.lock ''"
expect_plain "jq '.nodes.root' flake.lock" 'aitools json get flake.lock /nodes/root'
expect_plain "jq -r '.a[0].b_1' x.json" 'aitools json get x.json /a/0/b_1 --raw'
expect_plain "jq '.[10]' x.json" 'aitools json get x.json /10'
expect_plain "jq '.a.[2]' x.json" 'aitools json get x.json /a/2'
expect_plain 'ls' "aitools find '*' --depth 1 --no-ignore --glob '!.*' --limit 200"
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
expect_plain 'cat 2000' 'aitools read 2000 --max-lines 2000'
expect_plain 'cat 2000 | head -5' 'aitools read 2000 --range 1:5 --max-lines 5'

# Envelopes: a leading cd, a trailing stderr redirect, a trailing head/tail.
expect_plain 'cd src && cat a.txt' 'cd src && aitools read a.txt --max-lines 2000'
expect_plain 'cd ~/repo&&git status' 'cd ~/repo&&aitools git status'
expect_plain 'cat x 2>&1' 'aitools read x --max-lines 2000 2>&1'
expect_plain 'cat x 2>/dev/null' 'aitools read x --max-lines 2000 2>/dev/null'
expect_plain 'cat flake.nix | head -20' 'aitools read flake.nix --range 1:20 --max-lines 20'
expect_plain 'cat x | head -n 3' 'aitools read x --range 1:3 --max-lines 3'
expect_plain 'cat x | tail' 'aitools read x --tail 10 --max-lines 10'
expect_plain 'cat file | head' 'aitools read file --range 1:10 --max-lines 10'
expect_plain 'cat log | tail -5' 'aitools read log --tail 5 --max-lines 5'
expect_plain 'grep -rn foo src | head -20' 'aitools search foo src --context 0 --limit 20 --no-ignore'
expect_plain 'grep -rl foo src | head -500' 'aitools search foo src --output files --limit 200 --no-ignore'
expect_plain 'rg -n foo | head -3' "aitools search foo --context 0 --limit 3 $hidden"
expect_plain 'rg -v foo f | head -3' "aitools search foo f --invert --context 0 --limit 3 $hidden"
expect_plain 'rg --files | head -20' "aitools find '*' --type file $hidden --limit 20"
expect_plain 'fd Config | head -5' "aitools find Config $hidden --limit 5"
expect_plain 'cd src && rg -l foo' "cd src && aitools search foo --output files --limit 200 $hidden"
expect_plain 'cd .github && rg -l foo' ''
expect_plain 'cd .github && grep -rl foo .' 'cd .github && aitools search foo . --output files --limit 200 --no-ignore'
expect_plain 'ls | head' "aitools find '*' --depth 1 --no-ignore --glob '!.*' --limit 10"
expect_plain 'ls -A src' "aitools find '*' src --depth 1 --no-ignore --limit 200"
expect_plain 'ls .config' "aitools find '*' .config --depth 1 --no-ignore --glob '!.*' --limit 200"
expect_plain 'cd .github && ls' "cd .github && aitools find '*' --depth 1 --no-ignore --glob '!.*' --limit 200"
expect_plain 'ls -l' "aitools find '*' --depth 1 --no-ignore --glob '!.*' --limit 200"
expect_plain "find . -name '*.nix' | head -5" "aitools find '*.nix' . --no-ignore --limit 5"
expect_plain 'git log --oneline | head -5' 'aitools git log --limit 5'
expect_plain 'git show HEAD:f | head -30' 'aitools git show HEAD:f --max-lines 30'
expect_plain 'grep -rn foo . 2>/dev/null | head -5' 'aitools search foo . --context 0 --limit 5 --no-ignore 2>/dev/null'
expect_plain 'cd src && grep -rn foo . 2>&1 | head -5' 'cd src && aitools search foo . --context 0 --limit 5 --no-ignore 2>&1'

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
  'wc -w f.nix' \
  'wc f.nix' \
  'wc -c a b' \
  'rg foo .github' \
  'rg foo ../other' \
  'rg foo /abs/path' \
  'rg --files .config' \
  'cd .config && rg foo' \
  'cd /tmp && rg foo' \
  'cd ~/repo && rg foo' \
  'rg --hidden foo' \
  'rg -o -v foo f' \
  'rg -o -C 2 foo f' \
  'rg -ol foo' \
  'rg -vl foo' \
  'rg -vc foo' \
  'rg -Uv foo' \
  'rg -v -e a -e b f' \
  'rg -xw foo f' \
  "rg -x '  };' f" \
  "rg 'a{' f" \
  "rg '\${x}' f" \
  "grep -rE 'a{,2}' ." \
  'rg -x -e a -e b f' \
  'rg -t js foo' \
  'rg -t md foo' \
  'rg -t nix -t rust foo' \
  "rg -t nix -g '*.md' x" \
  "rg --files -t nix -g '*.md'" \
  "rg 'fn \\w+' -g '*.rs'" \
  "rg --files -g '*.nix'" \
  "rg -l foo -g '!src'" \
  "rg -l foo -g '!src/**'" \
  "rg -l foo -g '*.log'" \
  'fd -H -I Foo' \
  'fd -HI .' \
  'rg -oc foo f' \
  'cd .github && fd Foo' \
  'cd .github && tree' \
  'rg -T nix foo' \
  'rg --files a b' \
  'rg --files -t sh' \
  'rg --files --hidden' \
  'rg -o foo f | head -3' \
  'grep -o -v foo f' \
  'grep -rvl foo .' \
  'fd readme' \
  'fd -e nix' \
  'fd "a.b"' \
  'fd Foo src extra' \
  'fd -t l X' \
  'fd -t f -t d X' \
  'fd -g "*.NIX"' \
  'fd X .config' \
  'fd -d 0 X' \
  'tree -a' \
  'tree -I node_modules' \
  'tree a b' \
  'tree .github' \
  'tree | head -5' \
  "jq 'keys' x.json" \
  "jq '.[]' x.json" \
  "jq '.a | length' x.json" \
  "jq '[0]' x.json" \
  "jq '.[01]' x.json" \
  "jq '.[-1]' x.json" \
  "jq '.\"a b\"' x.json" \
  'jq . a.json b.json' \
  'jq .' \
  'jq -c . x.json' \
  'jq . x.json | head -3' \
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
  'cd $HOME && cat x' \
  'cd a b && cat x' \
  'cd src; cat x' \
  'cat x | head -0' \
  'cat x | head -c 5' \
  'cat x | grep y' \
  'cat x | head -5 | tail -1' \
  'cat x > out 2>&1' \
  'cat x 2>err' \
  "grep 'a | head -5' f" \
  'grep -rn -C2 foo src | head -3' \
  'grep -rn foo src | tail -3' \
  'rg foo | tail -3' \
  "sed -n '1,5p' f | head -2" \
  'head -5 x | tail -1' \
  'git status | head' \
  'git diff | head -20' \
  'git blame -L 20,10 f' \
  'git blame -L 1,99999999999999999999 f' \
  'git blame -L 1, f' \
  'git log | head -5' \
  'tail -5 x | head -3' \
  'tail -n 5 x | tail -2' \
  'cat x | head -99999999' \
  'git stash' \
  'git stash pop' \
  'git checkout main' \
  'git switch main' \
  'git reset --hard' \
  'git clean -fd' \
  'cd /tmp' \
  'cd /tmp && git stash' \
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

# aitools anchors its globs at the nearest directory holding .git, so a working directory below a
# dot-directory of that workspace must not get the hidden-entry globs, while the workspace top must.
repo="$stub_dir/repo"
mkdir -p "$repo/.git" "$repo/.hidden/sub" "$repo/src"
cwd_payload() { jq -cn --arg c "$1" --arg d "$2" '{tool_name: "Bash", tool_input: {command: $c}, cwd: $d}'; }
expect_json claude "$(cwd_payload 'rg -l foo' "$repo")" ".hookSpecificOutput.updatedInput.command == \"aitools search foo --output files --limit 200 $hidden\""
expect_json claude "$(cwd_payload 'rg -l foo' "$repo/src")" '.hookSpecificOutput.updatedInput.command | startswith("aitools search")'
expect_json claude "$(cwd_payload 'rg -l foo' "$repo/.hidden")" ''
expect_json claude "$(cwd_payload 'fd Foo' "$repo/.hidden/sub")" ''
expect_json claude "$(cwd_payload 'tree' "$repo/.hidden")" ''
expect_json codex "$(cwd_payload 'rg --files' "$repo/.hidden")" ''
expect_json claude "$(cwd_payload 'cat x' "$repo/.hidden")" '.hookSpecificOutput.updatedInput.command == "aitools read x --max-lines 2000"'
expect_json claude "$(cwd_payload 'fd -H Foo' "$repo/.hidden")" '.hookSpecificOutput.updatedInput.command == "aitools find Foo --limit 200"'

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
