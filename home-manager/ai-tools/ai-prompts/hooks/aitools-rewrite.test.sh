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
# git is never rewritten: the block-destructive-git hook rejects it.
expect_plain 'git status' ''
expect_plain 'git status -sb' ''
expect_plain 'git log --oneline -5' ''
expect_plain 'git log -n 3 -- flake.nix' ''
expect_plain 'git diff' ''
expect_plain 'git diff --cached --stat' ''
expect_plain 'git diff -- flake.nix' ''
expect_plain 'git show HEAD:flake.nix' ''
expect_plain 'git blame -L 10,20 flake.nix' ''
expect_plain '  cat flake.nix  ' 'aitools read flake.nix --max-lines 2000'
expect_plain 'cat 2000' 'aitools read 2000 --max-lines 2000'
expect_plain 'cat 2000 | head -5' 'aitools read 2000 --range 1:5 --max-lines 5'

# Envelopes: a leading cd, a trailing stderr redirect, a trailing head/tail.
expect_plain 'cd src && cat a.txt' 'cd src && aitools read a.txt --max-lines 2000'
expect_plain 'cd ~/repo&&git status' ''
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
expect_plain 'git log --oneline | head -5' ''
expect_plain 'git show HEAD:f | head -30' ''
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

# `aitools read` exits 2 (selection.no-match) on a file without lines, so an empty file keeps its
# shell command; a file with content, or one that does not exist yet, still gets the rewrite.
empty_dir="$stub_dir/emptyfiles"
mkdir -p "$empty_dir/sub"
: >"$empty_dir/empty.txt"
: >"$empty_dir/sub/empty.txt"
printf 'x\n' >"$empty_dir/full.txt"
for cmd in 'cat empty.txt' 'head empty.txt' 'head -n 5 empty.txt' 'tail -n 5 empty.txt' \
  'sed -n 1,5p empty.txt' 'cat empty.txt | head -5' 'cat empty.txt 2>&1' 'cd sub && head empty.txt'; do
  expect_json claude "$(cwd_payload "$cmd" "$empty_dir")" ''
  expect_json codex "$(cwd_payload "$cmd" "$empty_dir")" ''
done
expect_json claude "$(cwd_payload 'head full.txt' "$empty_dir")" '.hookSpecificOutput.updatedInput.command == "aitools read full.txt --range 1:10 --max-lines 10"'
expect_json claude "$(cwd_payload 'head missing.txt' "$empty_dir")" '.hookSpecificOutput.updatedInput.command == "aitools read missing.txt --range 1:10 --max-lines 10"'
expect_json claude "$(cwd_payload 'cd sub && head full.txt' "$empty_dir")" '.hookSpecificOutput.updatedInput.command | startswith("cd sub && aitools read")'

# A range that starts beyond the last line fails the same way (exit 2), so it also keeps the shell
# command. plain mode resolves paths against the hook's own working directory.
printf 'a\nb\nc\n' >"$empty_dir/three.txt"
printf 'a\nb\nc' >"$empty_dir/nonl.txt"
plain_in() {
  checked=$((checked + 1))
  local got
  got=$(cd "$1" && printf '%s' "$2" | bash "$hook" plain)
  [[ $got == "$3" ]] || fail "plain in [$1] [$2]: expected [$3], got [$got]"
}
for cmd in "sed -n 4p three.txt" "sed -n 10,20p three.txt" "sed -n '4,\$p' three.txt" "sed -n 4p nonl.txt" "cd sub && sed -n 1p empty.txt"; do
  expect_json claude "$(cwd_payload "$cmd" "$empty_dir")" ''
  expect_json codex "$(cwd_payload "$cmd" "$empty_dir")" ''
  plain_in "$empty_dir" "$cmd" ''
done
plain_in "$empty_dir" 'cat empty.txt' ''
plain_in "$empty_dir" 'head empty.txt' ''
plain_in "$empty_dir" 'sed -n 3p three.txt' 'aitools read three.txt --range 3:3 --max-lines 1'
plain_in "$empty_dir" 'sed -n 3p nonl.txt' 'aitools read nonl.txt --range 3:3 --max-lines 1'
plain_in "$empty_dir" 'sed -n 2,9p three.txt' 'aitools read three.txt --range 2:9 --max-lines 8'
plain_in "$empty_dir" 'sed -n 4p missing.txt' 'aitools read missing.txt --range 4:4 --max-lines 1'
expect_json claude "$(cwd_payload 'sed -n 3p three.txt' "$empty_dir")" '.hookSpecificOutput.updatedInput.command == "aitools read three.txt --range 3:3 --max-lines 1"'

# `aitools search` and `aitools find` fail with argument.invalid on a path outside the workspace root,
# while read, info, json get, and diff accept one. So only grep, rg, ls, find, fd, and tree keep their
# shell command for an absolute path elsewhere, a `..` that climbs out, a `~` path, or an outside
# `cd DIR`; a path inside the root (here $repo, which holds .git) is still rewritten.
plain_in "$repo/src" 'cat /etc/x' 'aitools read /etc/x --max-lines 2000'
plain_in "$repo/src" 'head ../x' 'aitools read ../x --range 1:10 --max-lines 10'
plain_in "$repo/src" 'wc -c /etc/x' 'aitools info /etc/x'
plain_in "$repo/src" 'jq . /tmp/x.json' "aitools json get /tmp/x.json ''"
plain_in "$repo/src" 'diff a /tmp/b' 'aitools diff a /tmp/b'
plain_in "$repo/src" 'cat ./x' 'aitools read ./x --max-lines 2000'
plain_in "$repo/src" 'grep foo src' 'aitools search foo src --context 0 --limit 100 --no-ignore'
plain_in "$repo/src" 'grep -n foo ../src/a.txt' 'aitools search foo ../src/a.txt --context 0 --limit 100 --no-ignore'
plain_in "$repo/src" "grep -rn foo $repo/src" "aitools search foo $repo/src --context 0 --limit 100 --no-ignore"
plain_in "$repo/src" 'ls ..' "aitools find '*' .. --depth 1 --no-ignore --glob '!.*' --limit 200"
plain_in "$repo/src" 'find ../src -name "*.nix"' "aitools find '*.nix' ../src --no-ignore --limit 200"
plain_in "$repo/src" 'cd .. && grep -rn foo src' 'cd .. && aitools search foo src --context 0 --limit 100 --no-ignore'
for cmd in \
  'grep foo /tmp/x' \
  'grep -n foo /etc/x' \
  'grep -rn foo /tmp' \
  'grep -rn foo ../..' \
  'grep -rn foo ../../x' \
  'grep -rn foo src ../..' \
  'grep -rn foo ./../..' \
  'ls /tmp' \
  'ls ../..' \
  'find /tmp -name x' \
  'find /tmp -name "*.x"' \
  'find ../.. -type f' \
  'rg foo /tmp' \
  'rg --files /tmp' \
  'fd Foo /tmp' \
  'fd Foo ../..' \
  'tree /tmp' \
  'tree ../..' \
  'cd /tmp && grep -rn foo .' \
  'cd /tmp && ls' \
  'cd ../.. && find . -name "*.x"' \
  'cd ~/repo && ls' \
  'grep foo "~/x"' \
  'ls "~/x"' \
  'cat "~/x"' \
  'grep foo ~/x' \
  'ls ~' \
  'jj status' \
  'nix build .#foo' \
  'cd src && jj log' \
  'catalog x' \
  'lsof -i'; do
  plain_in "$repo/src" "$cmd" ''
done

# A list of simple commands joined by `;` or `&&` is rewritten segment by segment; a segment that is
# not rewritable stays byte for byte. `cd DIR` segments stay and set the directory for later ones.
# grep, rg, and diff exit 1 where aitools exits 0, so they are rewritten before an `&&` only with a
# trailing head/tail (the pipeline then reports head's status).
plain_in "$repo/src" 'cat a && cat b' 'aitools read a --max-lines 2000 && aitools read b --max-lines 2000'
plain_in "$repo/src" 'cat a; cat b' 'aitools read a --max-lines 2000; aitools read b --max-lines 2000'
plain_in "$repo/src" 'cat a;' 'aitools read a --max-lines 2000;'
# After `cd DIR ;` the next segment runs even when the cd failed, so the directory is unknown.
expect_plain 'cd src; cat x' ''
plain_in "$repo/src" 'echo ---; cat x 2>&1; jj st' 'echo ---; aitools read x --max-lines 2000 2>&1; jj st'
plain_in "$repo/src" 'echo "a;b && c"; cat x' 'echo "a;b && c"; aitools read x --max-lines 2000'
plain_in "$repo/src" "echo 'a && b' && cat c" "echo 'a && b' && aitools read c --max-lines 2000"
plain_in "$repo/src" 'cat x | sort; cat y' 'cat x | sort; aitools read y --max-lines 2000'
plain_in "$repo/src" 'ls | head -3; cat x | head -2' "aitools find '*' --depth 1 --no-ignore --glob '!.*' --limit 3; aitools read x --range 1:2 --max-lines 2"
plain_in "$repo/src" 'cd .. && cat x; cat y' 'cd .. && aitools read x --max-lines 2000; aitools read y --max-lines 2000'
plain_in "$repo/src" 'cd /tmp && grep -rn foo .; cat x' 'cd /tmp && grep -rn foo .; aitools read x --max-lines 2000'
plain_in "$repo/src" 'cd .. ; cd src && grep -rn foo . | head -3' ''
plain_in "$repo/src" 'cd .. && cd src && grep -rn foo . | head -3' 'cd .. && cd src && aitools search foo . --context 0 --limit 3 --no-ignore'
plain_in "$repo/src" 'grep -n foo a.txt; echo hit' ''
plain_in "$repo/src" 'echo hit && grep -n foo a.txt' 'echo hit && aitools search foo a.txt --context 0 --limit 100 --no-ignore'
plain_in "$repo/src" 'grep -n foo a.txt | head -5 && echo hit' 'aitools search foo a.txt --context 0 --limit 5 --no-ignore && echo hit'
for cmd in \
  'grep -n foo a.txt && echo hit' \
  'rg -n foo a.txt && echo hit' \
  'diff a b && echo same' \
  'cat a || cat b' \
  'cat a & cat b' \
  'cat a |& cat b' \
  'cat a;; cat b' \
  'cat a && ' \
  '; cat a' \
  'cd a b; cat x' \
  'cd -; cat x' \
  'cd; cat x' \
  'pushd ..; cat x' \
  'source env.sh; cat x' \
  'for f in a b; do cat x; done' \
  'if cat a; then cat b; fi' \
  'while cat a; do cat b; done' \
  '(cat a; cat b)' \
  '{ cat a; cat b; }' \
  'cat a; cat b # c' \
  'cat a; cat <<EOF' \
  'cat a; echo $(cat b)' \
  'cat a; echo `cat b`' \
  'cat a; echo "$(cat b)"' \
  'cat a; echo \; cat b' \
  'cat a; find . -exec cat {} \; ' \
  'cat a; echo "unterminated'; do
  plain_in "$repo/src" "$cmd" ''
done

# rg --no-config only drops the user's config file, which aitools never reads.
expect_plain 'rg --no-config -n foo src' "aitools search foo src --context 0 --limit 100 $hidden"

# A GNU BRE becomes aitools' regex: \| \( \) \{ \} \+ \? turn into the bare operators. Bare + ? | ( ) { }
# are literals in a BRE, escaped letters and digits differ between greps, a backslash in a bracket is a
# literal, and a leading * is a literal, so those stay as typed.
bre="--context 0 --limit 100 --no-ignore"
expect_plain "grep -rn 'foo\\|bar' src" "aitools search 'foo|bar' src $bre"
expect_plain "grep -rn 'a\\(b\\)\\{2\\}' src" "aitools search 'a(b){2}' src $bre"
expect_plain "grep -rn 'a\\+b\\?' src" "aitools search 'a+b?' src $bre"
expect_plain "grep -rn 'a\\.b\\*' src" "aitools search 'a\\.b\\*' src $bre"
expect_plain "grep -rn '[[:alpha:]]x[^]a]' src" "aitools search '[[:alpha:]]x[^]a]' src $bre"
expect_plain "grep -rn 'a.*b' src" "aitools search 'a.*b' src $bre"
for cmd in \
  "grep -rn 'a+b' src" \
  "grep -rn 'a|b' src" \
  "grep -rn '(a)' src" \
  "grep -rn 'a{2}' src" \
  "grep -rn '\\bfoo' src" \
  "grep -rn '\\w' src" \
  "grep -rn '\\1' src" \
  "grep -rn '[\\.]' src" \
  "grep -rn '[abc' src" \
  "grep -rn '*a' src" \
  "grep -rn '\\(*a\\)' src" \
  "grep -rn '^*a' src" \
  "grep -rn 'a\\' src"; do
  expect_plain "$cmd" ''
done

# An unquoted `~` or `~/...` is the home directory. It is outside the workspace root, so grep, rg, ls,
# find, fd, and tree keep their command, while read, info, json get, and diff take it. `~user`, a
# tilde inside a word, a quoted tilde, and an unusable HOME stay as typed.
plain_home() {
  checked=$((checked + 1))
  local got
  got=$(printf '%s' "$2" | HOME="$1" bash "$hook" plain)
  [[ $got == "$3" ]] || fail "plain HOME=[$1] [$2]: expected [$3], got [$got]"
}
plain_home /home/test 'cat ~/x' 'aitools read /home/test/x --max-lines 2000'
plain_home /home/test 'cat ~' 'aitools read /home/test --max-lines 2000'
plain_home /home/test 'head -n 3 ~/.x' 'aitools read /home/test/.x --range 1:3 --max-lines 3'
plain_home /home/test 'diff ~/a b' 'aitools diff /home/test/a b'
plain_home /home/test 'cd ~/repo && cat x' 'cd ~/repo && aitools read x --max-lines 2000'
plain_home /home/test 'cd ~/repo && grep -rn foo .' ''
for cmd in 'grep foo ~/x' 'ls ~' 'ls ~/x' 'find ~ -name "*.x"' 'rg foo ~/x' 'tree ~' 'cat ~user/x' 'cat a~b' "cat '~/x'" 'cat "~/x"' 'cat ~/x ~/y'; do
  plain_home /home/test "$cmd" ''
done
plain_home '' 'cat ~/x' ''
plain_home relative 'cat ~/x' ''

# wc -l reads the info lines field, which counts an unterminated last line where wc does not, so the
# file must exist and be empty or end in a newline.
printf 'a\nb\n' >"$repo/src/nl.txt"
printf 'a\nb' >"$repo/src/nonl.txt"
: >"$repo/src/empty.txt"
plain_in "$repo/src" 'wc -l nl.txt' 'aitools info nl.txt'
plain_in "$repo/src" 'wc -l empty.txt' 'aitools info empty.txt'
plain_in "$repo/src" 'wc -l nonl.txt' ''
plain_in "$repo/src" 'wc -l missing.txt' ''
plain_in "$repo/src" 'wc -l src' ''
plain_in "$repo/src" 'wc -l nl.txt nonl.txt' ''
plain_in "$repo" 'cd src && wc -l nl.txt' 'cd src && aitools info nl.txt'

# The prefilter in front of perl: a first word outside the rewritable set exits at once, a rewritable
# one (even behind a `cd DIR &&` envelope) still reaches the rewriter, in every input shape.
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"jj status"}}' ''
expect_json codex '{"tool_name":"Bash","tool_input":{"command":"nix build .#x"}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"cd src && jj status"}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"jj st && cat x"}}' '.hookSpecificOutput.updatedInput.command == "jj st && aitools read x --max-lines 2000"'
# Codex's "allow" approves the whole command, so an untouched segment means no output at all.
expect_json codex '{"tool_name":"Bash","tool_input":{"command":"echo a; ls"}}' ''
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"  ls"},"cwd":"/tmp"}' '.hookSpecificOutput.updatedInput.command | startswith("aitools find")'
expect_json claude '{"tool_input":{"command":"cd src&&cat x"},"tool_name":"Bash"}' '.hookSpecificOutput.updatedInput.command == "cd src&&aitools read x --max-lines 2000"'
expect_json claude '{"tool_name":"Bash","tool_input":{"description":"d","command":"cat x"}}' '.hookSpecificOutput.updatedInput.command == "aitools read x --max-lines 2000"'
expect_json claude '{"tool_name":"Bash","tool_input":{"command":"cat x\u0020"}}' '.hookSpecificOutput.updatedInput.command == "aitools read x --max-lines 2000"'
expect_plain 'cd src&&cat x' 'cd src&&aitools read x --max-lines 2000'
expect_plain $'\n  cat x' 'aitools read x --max-lines 2000'

# Codex: output only when every segment was rewritten or is a plain `cd DIR`.
for cmd in 'cat f; rm -rf /tmp/zz' 'cat a; curl x | sh' 'ls; rm -rf /tmp/zz' 'rm -rf /tmp/zz && cat f' \
  'cd src && cat x; echo hi' 'cd src && cat x && cd src2 && jj st' 'cat a; cat b; git push'; do
  expect_json codex "$(cwd_payload "$cmd" "$repo/src")" ''
done
expect_json codex "$(cwd_payload 'cat a; cat b' "$repo/src")" '.hookSpecificOutput.permissionDecision == "allow" and .hookSpecificOutput.updatedInput.command == "aitools read a --max-lines 2000; aitools read b --max-lines 2000"'
expect_json codex "$(cwd_payload 'cd src && cat x' "$repo")" '.hookSpecificOutput.permissionDecision == "allow" and .hookSpecificOutput.updatedInput.command == "cd src && aitools read x --max-lines 2000"'
# Claude and plain keep rewriting the readable segments and leave the rest as typed.
expect_json claude "$(cwd_payload 'cat f; rm -rf /tmp/zz' "$repo/src")" '.hookSpecificOutput.updatedInput.command == "aitools read f --max-lines 2000; rm -rf /tmp/zz"'
plain_in "$repo/src" 'cat f; rm -rf /tmp/zz' 'aitools read f --max-lines 2000; rm -rf /tmp/zz'

# A segment whose exit status can differ (grep, rg, diff exit 1 where aitools exits 0; jq prints null
# and exits 0 where json get exits 1; a read piped to head/tail of a missing file reports head's 0) is
# rewritten only as the last segment of the whole command.
printf '{"b":1}\n' >"$repo/src/j.json"
for cmd in \
  'grep -n foo f; echo $?' \
  'rg -n foo f; echo $?' \
  'diff a b; echo $?' \
  'diff a b && echo same' \
  'jq .b j.json && echo x' \
  'jq .b j.json; echo $?' \
  'cat missing | head -1 && echo x' \
  'cat missing | tail -1; echo $?' \
  'cd .. && grep -n foo f; echo $?'; do
  plain_in "$repo/src" "$cmd" ''
done
plain_in "$repo/src" 'echo x && jq .b j.json' 'echo x && aitools json get j.json /b'
plain_in "$repo/src" 'cat a; jq .b j.json' 'aitools read a --max-lines 2000; aitools json get j.json /b'
plain_in "$repo/src" 'cat missing | head -1' 'aitools read missing --range 1:1 --max-lines 1'
plain_in "$repo/src" 'cat nl.txt | head -1 && echo x' 'aitools read nl.txt --range 1:1 --max-lines 1 && echo x'
plain_in "$repo/src" 'grep -n foo f | head -2; echo $?' 'aitools search foo f --context 0 --limit 2 --no-ignore; echo $?'
plain_in "$repo/src" 'diff a b | head -2 && echo x' ''

# cd tracking: the directory accumulates across `&&`-joined cds, an option or `~user` is not a
# directory, and `;` after a cd leaves the directory unknown.
plain_in "$repo/src" 'cd .. && cd .. && grep -n foo f' ''
plain_in "$repo/src" 'cd .. && cd src && grep -n foo f' 'cd .. && cd src && aitools search foo f --context 0 --limit 100 --no-ignore'
plain_in "$repo" 'cd src && cd sub && grep -n foo f' 'cd src && cd sub && aitools search foo f --context 0 --limit 100 --no-ignore'
plain_in "$repo" 'cd /tmp && cd x && grep -rn foo .' ''
plain_in "$repo" "cd $repo/src && grep -n foo f" "cd $repo/src && aitools search foo f --context 0 --limit 100 --no-ignore"
plain_in "$repo" "cd /tmp; cd $repo/src; grep -n foo f" ''
plain_in "$repo" "cd $repo/src; grep -n foo f" ''
plain_in "$repo" 'cd -foo && grep -n foo f' ''
plain_in "$repo" 'cd -foo && cat f' ''
plain_home /home/test 'cd ~user && cat x' ''
plain_home /home/test 'cd ~ && cat x' 'cd ~ && aitools read x --max-lines 2000'
# After HOME changes, `~` no longer means the HOME the hook sees.
plain_home /home/test 'HOME=/other; cat ~/x' ''
plain_home /home/test 'export HOME=/other && cat ~/x' ''
plain_home /home/test 'export HOME=/other && cd ~/r && cat x' ''
plain_home /home/test 'unset HOME; cat ~/x' ''
plain_home /home/test 'echo a; cat ~/x' 'echo a; aitools read /home/test/x --max-lines 2000'
plain_home /home/test 'export HOME=/other && cat x' 'export HOME=/other && aitools read x --max-lines 2000'

# BRE conversion: `$` and `^` are anchors only in anchoring positions (elsewhere literal), \{, is
# a syntax error for aitools, a quantifier cannot follow a quantifier, and empty branches or groups
# differ between the dialects.
d='$'
expect_plain "grep -rn 'a\\|b${d}\\|c' src" "aitools search 'a|b\$|c' src $bre"
expect_plain "grep -rn '^a\\|^b' src" "aitools search '^a|^b' src $bre"
expect_plain "grep -rn '\\(^a${d}\\)' src" "aitools search '(^a\$)' src $bre"
expect_plain "grep -rn '^a${d}\\|b' src" "aitools search '^a\$|b' src $bre"
expect_plain "grep -rn 'a\\{1,2\\}' src" "aitools search 'a{1,2}' src $bre"
expect_plain "grep -rn 'x\\(ab\\)*' src" "aitools search 'x(ab)*' src $bre"
for cmd in \
  "grep -rn 'a\$b' src" \
  "grep -rn '\$a' src" \
  "grep -rn 'a^b' src" \
  "grep -rn '^^a' src" \
  "grep -rn 'a\\(^b\\)\\|c^' src" \
  "grep -rn 'a\\{,2\\}' src" \
  "grep -rn 'a\\?\\?' src" \
  "grep -rn 'a\\+\\?' src" \
  "grep -rn 'x\\{1\\}\\{2\\}' src" \
  "grep -rn 'a**' src" \
  "grep -rn 'a*\\{2\\}' src" \
  "grep -rn 'a\\|' src" \
  "grep -rn '\\|a' src" \
  "grep -rn 'a\\|\\|b' src" \
  "grep -rn '\\(\\)' src" \
  "grep -rn '\\(a\\|\\)' src" \
  "grep -rn '\\(\\|a\\)' src" \
  "grep -rn '\\?a' src" \
  "grep -rn '^\\{2\\}' src"; do
  expect_plain "$cmd" ''
done

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
