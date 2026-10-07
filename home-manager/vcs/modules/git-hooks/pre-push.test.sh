#!/bin/bash
# Scenario test for the gitHooks pre-push script, run by the `git-hooks-pre-push` flake check.
# Usage: pre-push.test.sh <path-to-pre-push>
#
# The hook is invoked directly with the arguments and ref-update lines git would pass it, against a
# local bare remote, so no network or real push is involved. The identity check stands aside here
# (no ghq.root is configured), which leaves the gitleaks scan as the behavior under test.

set -euo pipefail

hook="$1"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

export HOME="$PWD/home"
mkdir -p "$HOME"
git config --global user.name test
git config --global user.email test@example.com
git config --global init.defaultBranch main

git init -q --bare remote.git
git init -q work
cd work

zero=0000000000000000000000000000000000000000

# expect <exit status> <description> [<local oid> <remote oid>]; with no oids the hook gets no ref
# updates. Not fed through a pipe, which would run it in a subshell and lose the counters.
expect() {
  checked=$((checked + 1))
  local status=0 updates=
  [[ $# -eq 4 ]] && updates="refs/heads/main $3 refs/heads/main $4"
  bash "$hook" origin ../remote.git <<<"$updates" >/dev/null 2>&1 || status=$?
  [[ $status -eq $1 ]] || fail "$2: expected exit $1, got $status"
}

commit() {
  printf '%s\n' "$2" >"$1"
  git add "$1"
  git commit -q -m "$1"
  git rev-parse HEAD
}

# Split so this file does not itself carry a token gitleaks would flag.
token="gh""p_Zk3VbQ9xLm2RtY7uWp4NcA8sHd6FjE1gKo5T"

git remote add origin ../remote.git

clean=$(commit clean.txt 'nothing secret')
expect 0 "clean commit to an empty remote" "$clean" "$zero"

leak=$(commit leak.txt "token = \"$token\"")
expect 1 "leaked token on a first push" "$leak" "$zero"

git push -q --no-verify origin "$clean:refs/heads/main"
expect 1 "leaked token in the range the remote lacks" "$leak" "$clean"

git push -q --no-verify origin "$leak:refs/heads/main"
after=$(commit after.txt 'still nothing secret')
expect 0 "leak already on the remote is not rescanned" "$after" "$leak"

expect 0 "branch deletion" "$zero" "$clean"

expect 0 "no ref updates"

echo "pre-push: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
