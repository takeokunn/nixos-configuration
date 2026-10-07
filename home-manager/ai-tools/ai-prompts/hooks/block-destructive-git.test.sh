#!/bin/bash
# Table-driven test for block-destructive-git.sh, run by the `block-destructive-git-hook` flake check.
# Usage: block-destructive-git.test.sh <path-to-block-destructive-git.sh>

set -euo pipefail

hook="$1"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

stderr_file=$(mktemp)
trap 'rm -f "$stderr_file"' EXIT

run_hook() {
  jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' | bash "$hook" >/dev/null 2>"$stderr_file"
}

# A crash that happened to exit 2 would pass on status alone, so a block must also print the banner.
expect_block() {
  checked=$((checked + 1))
  local status=0
  run_hook "$1" || status=$?
  [[ $status -eq 2 ]] || fail "expected block (exit 2) for [$1], got exit $status"
  grep -q 'ORCH-P005' "$stderr_file" || fail "block for [$1] printed no ORCH-P005 banner"
}

expect_allow() {
  checked=$((checked + 1))
  local status=0
  run_hook "$1" || status=$?
  [[ $status -eq 0 ]] || fail "expected allow (exit 0) for [$1], got exit $status"
  [[ ! -s $stderr_file ]] || fail "allow for [$1] wrote to stderr: $(cat "$stderr_file")"
}

# git: the pre-existing rules.
expect_block 'git stash'
expect_block 'git stash push -m wip'
expect_block 'git switch main'
expect_block 'git reset --hard HEAD~1'
expect_block 'git clean -fd'
expect_block 'git checkout main'
expect_block 'bash -c "git stash"'
expect_block 'cd repo && git -C . switch main'
expect_allow 'git stash list'
expect_allow 'git reset --soft HEAD~1'
expect_allow 'git checkout -b feat/x'
expect_allow 'git checkout -- flake.nix'
expect_allow 'git status'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 git stash'

# jj: commands that rewrite the shared working copy.
expect_block 'jj edit abc'
expect_block 'jj next'
expect_block 'jj prev --edit'
expect_block 'jj abandon'
expect_block 'jj abandon xyz'
expect_block 'jj undo'
expect_block 'jj redo'
expect_block 'jj op restore 1234'
expect_block 'jj operation revert 1234'
expect_block 'jj restore'
expect_block 'jj restore --from main'
expect_block 'jj restore -R .'
expect_block 'jj new main'
expect_block 'jj new -A main'
expect_block 'jj new --insert-before=main'
expect_block 'jj --config ui.color=never edit abc'
expect_block '/nix/store/x-jujutsu/bin/jj edit abc'
expect_block "bash -c 'jj abandon'"
expect_block 'echo ok; jj undo'
expect_block 'jj restore --into main'
expect_block 'jj new -r somebranch'
expect_block 'jj new main -r foo'
expect_block 'jj new @ other'
expect_block 'jj --config=ui.color=never edit abc'
expect_allow 'jj new'
expect_allow 'jj new @'
expect_allow 'jj new -r @'
expect_allow 'jj new -o @'
expect_allow 'jj new -m "start feature"'
expect_allow 'jj new main --no-edit'
expect_allow 'jj restore flake.nix'
expect_allow 'jj restore --from main -- flake.nix'
expect_allow 'jj restore -R . flake.nix'
expect_allow 'jj op log'
expect_allow 'jj edit --help'
expect_allow 'jj log -r @'
expect_allow 'jj status'
expect_allow 'jj describe -m "feat: x"'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon'

# jj git push skips the gitleaks pre-commit hook, so it needs the override.
expect_block 'jj git push'
expect_block 'jj git push -b main'
expect_block 'jj -R . git push --all'
expect_allow 'jj git push --dry-run'
expect_allow 'jj git fetch'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'

# A value the shell computes at run time cannot be proven safe.
expect_block 'jj new $(echo main)'
expect_block 'jj new `echo main`'
expect_block 'jj $(echo abandon)'
expect_block 'jj $SUBCOMMAND'
expect_block 'jj op $(echo restore) 1234'
expect_block 'jj git $(echo push)'
expect_block 'jj git push -b $(jj log -r @- -T bookmarks)'
expect_allow 'jj describe -m "$(cat msg.txt)"'
expect_allow 'jj log -r $(echo main)'
expect_allow 'echo $(jj log)'

expect_block 'git $(echo stash)'
expect_block 'git $SUB'
expect_block 'git reset $(echo --hard)'
expect_block 'git clean $FLAGS'
expect_allow 'git log $(git merge-base HEAD main)..HEAD'
expect_allow 'git commit -m "$(cat msg.txt)"'

# A jj name that is not a built-in may be an alias for anything.
expect_block 'jj ab'
expect_block 'jj --config aliases.x=["abandon"] x'
# The default aliases live in config, so a config file can repoint them.
expect_block 'jj b list'
expect_block 'jj ci -m "feat: x"'
expect_block 'jj desc -m "feat: x"'
expect_block 'jj st'
expect_allow 'jj bookmark list'
expect_allow 'jj op log'
expect_allow 'jj obslog'
expect_allow 'jj evolution-log'
expect_allow 'jj debug revset @'

# Overrides that defeat the private-commits author check.
expect_block 'jj --config user.email=x@example.com git push -b main'
expect_block 'jj --config=git.private-commits=none() log'
expect_block 'jj --config-file other.toml log'
expect_block 'jj git push --allow-private -b main'
expect_block 'JJ_EMAIL=x@example.com jj describe -m x'
expect_block 'env JJ_USER=x jj log'
expect_allow 'jj --config ui.color=never log'

# Skipping or redirecting the Git hooks skips the secret and identity checks.
expect_block 'git commit --no-verify -m x'
expect_block 'git commit -nm x'
expect_block 'git push --no-verify origin main'
expect_block 'git -c core.hooksPath=/dev/null push origin main'
expect_block 'git -c core.hooksPath=/tmp/none commit -m x'
expect_block 'GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=core.hooksPath GIT_CONFIG_VALUE_0=/tmp git push'
expect_block 'git config core.hooksPath /tmp/none'
expect_allow 'git config --get core.hooksPath'
expect_allow 'git push -n origin main'
expect_allow 'git commit -m "no verify needed"'
expect_allow 'git -c color.ui=never log'

# Wrappers whose operands are not the command.
expect_block 'exec -a name jj abandon'
expect_block 'exec -a name git stash'
expect_block 'env -S "jj abandon"'
expect_block "env -S'git stash'"
expect_allow 'jj'
expect_allow 'jj --version'

# Nesting past the recursion limit still runs the innermost command.
expect_block 'eval eval eval eval eval jj abandon'
expect_block 'eval eval eval eval eval git stash'
expect_block 'eval eval eval eval eval echo ok'
expect_allow 'eval eval eval eval jj log'
expect_block 'eval eval eval eval jj abandon'
expect_block 'eval eval eval eval git stash'
nested='ls'
for _ in 1 2 3 4; do nested="bash -c $(printf '%q' "$nested")"; done
expect_allow "$nested"
for _ in 5 6; do nested="bash -c $(printf '%q' "$nested")"; done
expect_block "$nested"

# jj util exec runs its operand as a command.
expect_block 'jj util exec -- git stash'
expect_block 'jj util exec -- bash -c "jj abandon"'
expect_allow 'jj util exec -- git status'

# Data, not invocations.
expect_allow 'echo "jj abandon"'
expect_allow "rg -n 'jj undo' docs"
expect_allow $'cat <<EOF\njj abandon\nEOF'

# Other tools stand aside.
checked=$((checked + 1))
status=0
jq -n '{tool_name: "Read", tool_input: {command: "jj abandon"}}' | bash "$hook" >/dev/null 2>&1 || status=$?
[[ $status -eq 0 ]] || fail "non-Bash tool: expected exit 0, got $status"

echo "block-destructive-git: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
