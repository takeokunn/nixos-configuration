#!/usr/bin/env bash
# Usage: bash statusline.test.sh <statusline.sh>
#
# The branch segment must describe the directory's own jj workspace: inside a
# jj workspace of a bare repository, git resolves to the bare root and would
# report that root's HEAD instead.

set -u

statusline=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
total=0
failures=0

check() {
  local name=$1 expected=$2 actual=$3
  total=$((total + 1))
  if [[ $actual == *"$expected"* ]]; then
    echo "PASS: $name"
  else
    echo "FAIL: $name (expected '$expected' in '$actual')"
    failures=$((failures + 1))
  fi
}

check_absent() {
  local name=$1 unexpected=$2 actual=$3
  total=$((total + 1))
  if [[ $actual != *"$unexpected"* ]]; then
    echo "PASS: $name"
  else
    echo "FAIL: $name (unexpected '$unexpected' in '$actual')"
    failures=$((failures + 1))
  fi
}

root=$(mktemp -d)
trap 'rm -rf "$root"' EXIT

export HOME=$root/home
mkdir -p "$HOME"
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=$root/gitconfig
export JJ_CONFIG=$root/jjconfig JJ_USER=tester JJ_EMAIL=tester@example.com
export GIT_AUTHOR_NAME=tester GIT_AUTHOR_EMAIL=tester@example.com
export GIT_COMMITTER_NAME=tester GIT_COMMITTER_EMAIL=tester@example.com
touch "$GIT_CONFIG_GLOBAL" "$JJ_CONFIG"

input='{"model":{"display_name":"M"},"workspace":{"current_dir":"/x"}}'
run() { (cd "$1" && printf '%s' "$input" | bash "$statusline"); }

git init -q -b main "$root/origin"
git -C "$root/origin" commit -q --allow-empty -m one
git -C "$root/origin" branch feat
git -C "$root/origin" commit -q --allow-empty -m two
git clone -q --bare "$root/origin" "$root/r.git"

# The bare-root jj layout __fzf_ghq_init_jj builds.
mkdir "$root/r.git/.jj-init"
jj git init --git-repo "$root/r.git" "$root/r.git/.jj-init" >/dev/null 2>&1
jj -R "$root/r.git/.jj-init" sparse set --clear >/dev/null 2>&1
mv "$root/r.git/.jj-init/.jj" "$root/r.git/.jj"
rmdir "$root/r.git/.jj-init"
printf %s ../../.. >"$root/r.git/.jj/repo/store/git_target"

mkdir -p "$root/r.git/.worktrees"
jj -R "$root/r.git" workspace add --sparse-patterns full --name feat -r feat \
  "$root/r.git/.worktrees/feat" >/dev/null 2>&1
jj -R "$root/r.git" workspace add --sparse-patterns full --name bare -r 'root()' \
  "$root/r.git/.worktrees/bare" >/dev/null 2>&1

out=$(run "$root/r.git/.worktrees/feat")
check "jj workspace shows its parent's bookmark" " feat" "$out"
check_absent "jj workspace does not show the bare root's HEAD" "main" "$out"

change_id=$(jj -R "$root/r.git/.worktrees/bare" --ignore-working-copy log --no-graph -r @ -T 'change_id.short(8)')
out=$(run "$root/r.git/.worktrees/bare")
check "jj workspace without bookmarks shows its change id" "@ ($change_id)" "$out"

mkdir -p "$root/r.git/.worktrees/feat/sub/dir"
out=$(run "$root/r.git/.worktrees/feat/sub/dir")
check "a subdirectory finds the enclosing jj workspace" " feat" "$out"

out=$(run "$root/origin")
check "git repository shows its branch" " main" "$out"

mkdir "$root/plain"
# Without a branch segment the line has two " | " separators instead of three.
out=$(run "$root/plain")
separators=$(grep -o ' | ' <<<"$out" | wc -l | tr -d ' ')
check "a directory outside any repository shows no branch" "2" "$separators"

expected_checks=6
echo "$total checks, $failures failed"
if [ "$total" -ne "$expected_checks" ]; then
  echo "FAIL: ran $total checks, expected $expected_checks"
  exit 1
fi
[ "$failures" -eq 0 ]
