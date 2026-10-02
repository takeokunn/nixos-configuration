#!/bin/bash
# Table-driven test for gh-secret-guard.sh, run by the `gh-secret-guard` flake check.
# Usage: gh-secret-guard.test.sh <guard built against stub-gh> <the same guard with a missing gitleaks>
#
# The stub gh records its arguments and standard input under $STUB_LOG, prints $STUB_TOKEN for
# `auth token`, and exits with $STUB_EXIT. Fake secrets are generated at run time: a literal one in
# this file would be rejected by the repository's gitleaks pre-commit hook.

set -uo pipefail

guard="$1"
broken_guard="$2"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

work=$(mktemp -d)
trap 'chmod -R u+rwx "$work"; rm -rf "$work"' EXIT
cd "$work" || exit 1
export STUB_LOG="$work/stub"

random() { perl -e 'my @c = ("A" .. "Z", "a" .. "z", 0 .. 9); print map { $c[rand @c] } 1 .. shift' "$1"; }

# gitleaks' github-oauth rule needs gho_ plus 36 alphanumerics above an entropy threshold.
token="gho_$(random 36)"
# Formats gitleaks does not flag, caught only by the exact-match check.
export STUB_TOKEN="plainaccounttoken$(random 8)"
export MY_SERVICE_API_KEY="plainservicekey$(random 8)"

mkdir -p ghconfig
export GH_CONFIG_DIR="$work/ghconfig"
printf 'github.com:\n    users:\n        someone:\n    user: someone\n' >ghconfig/hosts.yml

printf 'Verification: NODE_AUTH_TOKEN=%s pnpm install\n' "$token" >leaky.md
printf 'Verification: pnpm install exited 0\n' >clean.md
cp clean.md unreadable.md
chmod 000 unreadable.md

# Sets $status; stderr goes to $work/err.
run() {
  local input=$1
  shift
  rm -rf "$STUB_LOG"
  status=0
  "$guard" "$@" <"$input" >/dev/null 2>"$work/err" || status=$?
}

expect_block() {
  local input=$1
  shift
  checked=$((checked + 1))
  run "$input" "$@"
  [[ $status -eq 1 ]] || fail "expected block (exit 1) for [$*], got $status"
  grep -q 'gh refused to run' "$work/err" || fail "block for [$*] printed no banner"
  [[ ! -e $STUB_LOG/argv ]] || fail "block for [$*] still ran gh"
  for s in "$token" "$STUB_TOKEN" "$MY_SERVICE_API_KEY"; do
    if grep -qF "$s" "$work/err"; then fail "block for [$*] echoed a secret"; fi
  done
}

expect_allow() {
  local input=$1
  shift
  checked=$((checked + 1))
  run "$input" "$@"
  [[ $status -eq 0 ]] || fail "expected allow for [$*], got $status: $(cat "$work/err")"
  [[ -e $STUB_LOG/argv ]] || fail "allow for [$*] did not run gh"
}

# --- Secrets in arguments (gitleaks) ---

expect_block /dev/null pr review 77 --comment --body "Verification: NODE_AUTH_TOKEN=$token pnpm install"
grep -q 'github-oauth' "$work/err" || fail "gitleaks block does not name the rule"
expect_block /dev/null api repos/o/r/issues/1/comments -f "body=$token"
expect_block /dev/null issue create --title "$token" --body x
expect_block /dev/null pr comment 1 --body "$token # gitleaks:allow"

# --- Secrets this machine holds (exact match) ---

expect_block /dev/null pr comment 1 --body "token: $STUB_TOKEN"
grep -q 'credential held on this machine' "$work/err" || fail "exact-match block does not explain itself"
expect_block /dev/null api user -H "Authorization: Bearer $MY_SERVICE_API_KEY"
printf 'key %s\n' "$MY_SERVICE_API_KEY" >held.md
expect_block /dev/null pr comment 1 --body-file held.md

# --- Files gh reads ---

expect_block /dev/null pr review 77 --body-file leaky.md
expect_block /dev/null pr review 77 --body-file=leaky.md
expect_block /dev/null pr create -F leaky.md --title t
expect_block /dev/null pr create -Fleaky.md --title t
expect_block /dev/null pr create -d -F leaky.md --title t
expect_block /dev/null issue comment 1 -F "$work/leaky.md"
expect_block /dev/null release create v1 --notes-file leaky.md
expect_block /dev/null release upload v1 "leaky.md#notes"
expect_block /dev/null api repos/o/r/issues/1/comments -F body=@leaky.md
expect_block /dev/null api gists -F 'files[x][content]=@leaky.md'
expect_block /dev/null api repos/o/r/issues/1/comments --input leaky.md
expect_block /dev/null gist create clean.md leaky.md
expect_block /dev/null gist edit abc123 -a leaky.md

# --- Standard input gh reads ---

expect_block leaky.md pr comment 1 --body-file -
expect_block leaky.md pr comment 1 -F -
expect_block leaky.md pr comment 1 --body-file=-
expect_block leaky.md pr comment 1 -F-
expect_block leaky.md pr comment 1 --body-file=/dev/stdin
expect_block leaky.md pr comment 1 --body-file /dev/stdin
expect_block leaky.md api repos/o/r/issues/1/comments -F body=@-
expect_block leaky.md api repos/o/r/issues/1/comments --input -
expect_block leaky.md gist create
expect_block leaky.md gist create -f notes.md

# --- Commit and tag messages gh can turn into a body, however the flags are spelled ---

# Isolated from the caller's git config, which may require hooks or commit signing.
commit() {
  git -C "$1" -c user.name=t -c user.email=t@example.com -c core.hooksPath=/dev/null -c commit.gpgsign=false \
    commit -q --allow-empty -m "$2" || fail "could not create a fixture commit in $1"
}
git init -q repo
git init -q clean-repo
commit repo "debug: NODE_AUTH_TOKEN=$token"
commit clean-repo "chore: clean"
git -C repo -c user.name=t -c user.email=t@example.com -c tag.gpgsign=false tag -a v1 -m "debug: NODE_AUTH_TOKEN=$token"

cd repo || exit 1
expect_block /dev/null pr create --fill
expect_block /dev/null pr create -fd
expect_block /dev/null pr -R o/r create --fill
expect_block /dev/null release create v1 --notes-from-tag
expect_block /dev/null release create v1 --notes-from-tag=true
expect_allow /dev/null pr view 1
cd ../clean-repo || exit 1
expect_allow /dev/null pr create --fill
cd "$work" || exit 1

# --- Aliases run later with arguments the guard never sees ---

expect_block /dev/null alias set p "api --input leaky.md"
expect_block /dev/null alias import aliases.yml

# --- Fail closed ---

expect_block /dev/null pr comment 1 --body-file unreadable.md
expect_block /dev/null pr comment 1 --body-file <(cat clean.md)
checked=$((checked + 1))
status=0
"$broken_guard" pr comment 1 --body "pnpm install exited 0" </dev/null >/dev/null 2>"$work/err" || status=$?
[[ $status -eq 1 ]] || fail "expected the broken scanner to block, got $status"
grep -q 'did not complete' "$work/err" || fail "broken scanner block did not explain itself"

# --- Allowed, and passed through unchanged ---

expect_allow /dev/null pr comment 1 --body "pnpm install exited 0"
[[ $(cat "$STUB_LOG/argv") == "$(printf '%s\n' pr comment 1 --body "pnpm install exited 0")" ]] ||
  fail "arguments were not passed through unchanged"

expect_allow /dev/null pr review 77 --body-file clean.md
expect_allow clean.md pr comment 1 --body-file -
cmp -s clean.md "$STUB_LOG/stdin" || fail "standard input did not reach gh intact"
expect_allow clean.md pr comment 1 --body-file /dev/stdin
cmp -s clean.md "$STUB_LOG/stdin" || fail "/dev/stdin did not reach gh intact"
expect_allow clean.md gist create
cmp -s clean.md "$STUB_LOG/stdin" || fail "gist create did not receive its standard input"

expect_allow /dev/null pr comment 1 --body-file missing.md
expect_allow /dev/null pr view 77 --json body --jq .body
expect_allow /dev/null api repos/o/r -f "title=fix: refresh token handling"
# Short and multi-line values of secret-named variables must not block ordinary text.
MY_SHORT_TOKEN=abc MULTI_LINE_TOKEN=$(printf "first line of a value\n\nlast line") expect_allow /dev/null pr comment 1 --body "abc and hello"

# With a file argument gist create does not read stdin, so the guard must not either: reading it
# would stall on a pipe that stays open. The leaky stdin proves it was left unread.
expect_allow leaky.md gist create clean.md

expect_allow /dev/null pr comment 1 --body 'Set it with NODE_AUTH_TOKEN=$(gh auth token), never the value'

# auth and secret carry credentials by design and are not scanned.
printf '%s\n' "$token" >token.txt
expect_allow token.txt auth login --with-token
cmp -s token.txt "$STUB_LOG/stdin" || fail "auth did not receive its standard input"
expect_allow /dev/null secret set NAME --body "$token"

# gh's exit status reaches the caller.
checked=$((checked + 1))
status=0
STUB_EXIT=4 "$guard" pr view 1 </dev/null >/dev/null 2>&1 || status=$?
[[ $status -eq 4 ]] || fail "expected gh's exit status 4 to pass through, got $status"

if [[ $failures -gt 0 ]]; then
  echo "$failures failures across $checked cases" >&2
  exit 1
fi
[[ $checked -gt 0 ]] || {
  echo "FAIL: no cases ran" >&2
  exit 1
}
echo "all $checked cases passed"
