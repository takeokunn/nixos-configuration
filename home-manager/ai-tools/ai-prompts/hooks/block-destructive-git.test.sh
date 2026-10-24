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

# git: read-only subcommands pass, everything else blocks.
expect_allow 'git status'
expect_allow 'git log --oneline -5'
expect_allow 'git diff HEAD~1'
expect_allow 'git show HEAD:flake.nix'
expect_allow 'git blame flake.nix'
expect_allow 'git rev-parse --show-toplevel'
expect_allow 'git ls-files'
expect_allow 'git ls-remote origin'
expect_allow 'git ls-tree HEAD'
expect_allow 'git rev-list --count HEAD'
expect_allow 'git cat-file -p HEAD'
expect_allow 'git grep -n foo'
expect_allow 'git describe --tags'
expect_allow 'git shortlog -sn'
expect_allow 'git name-rev HEAD'
expect_allow 'git for-each-ref refs/heads'
expect_allow 'git show-ref'
expect_allow 'git count-objects -v'
expect_allow 'git check-ignore x'
expect_allow 'git log --no-merges --format=%H --exit-code'
expect_allow 'git diff --no-ext-diff --name-only'
expect_allow 'git branch --no-contains abc'
expect_allow 'git merge-base HEAD main'
expect_allow 'git -C dir status'
expect_allow 'git -C . log'
expect_allow 'git --no-pager log'
expect_allow 'git -c color.ui=never log'
expect_allow 'git --git-dir=x.git --work-tree=. status'
expect_allow 'git --git-dir x.git status'
expect_allow '/usr/bin/git log'
expect_allow 'git'
expect_allow 'git --version'
expect_allow 'git --help'
expect_allow 'git log $(git merge-base HEAD main)..HEAD'
expect_allow 'git remote -v'
expect_allow 'git remote'
expect_allow 'git remote show origin'
expect_allow 'git remote get-url origin'
expect_allow 'git config --get user.name'
expect_allow 'git config --get-all remote.origin.url'
expect_allow 'git branch'
expect_allow 'git branch --list'
expect_allow 'git branch -a'
expect_allow 'git branch -r'
expect_allow 'git branch --show-current'
expect_allow 'git branch --contains abc'
expect_allow 'git branch -v'
expect_allow 'git tag'
expect_allow 'git tag -l'
expect_allow 'git tag --list "v*"'
expect_allow 'git stash list'
expect_allow 'git worktree list'
expect_allow 'bash -c "git diff"'
expect_allow 'sudo git status'
expect_allow 'env git status'
expect_allow 'xargs git log'
expect_allow 'eval git status'
expect_allow 'echo ok; git status'
expect_allow 'aitools git status'
expect_allow 'aitools git log'
expect_allow 'aitools --workspace . git log'
expect_allow 'aitools git diff --stat'
expect_allow 'aitools git log --grep run'
expect_allow 'aitools run -- git status'
expect_allow '! git status'
expect_allow 'if true; then git status; fi'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 git status'
expect_block 'git commit -m "no verify needed"'
expect_block 'git commit --amend'
expect_block 'git push'
expect_block 'git push origin main'
expect_block 'git reset --hard'
expect_block 'git reset --soft HEAD~1'
expect_block 'git checkout main'
expect_block 'git checkout -b feat/x'
expect_block 'git switch main'
expect_block 'git stash'
expect_block 'git stash push'
expect_block 'git stash pop'
expect_block 'git stash drop'
expect_block 'git clean -fd'
expect_block 'git add .'
expect_block 'git rm x'
expect_block 'git mv a b'
expect_block 'git restore x'
expect_block 'git rebase main'
expect_block 'git merge feat'
expect_block 'git cherry-pick abc'
expect_block 'git revert abc'
expect_block 'git pull'

# fetch: a named remote with plain refspecs passes; anything that could run a program, delete or
# rewrite a local ref, or write a local branch blocks.
expect_allow 'git fetch'
expect_allow 'git fetch --all'
expect_allow 'git fetch origin'
expect_allow 'git fetch origin main'
expect_allow 'git fetch origin main v1.0 refs/heads/x'
expect_allow 'git fetch -q origin'
expect_allow 'git fetch --no-tags origin main'
expect_allow 'git fetch origin +refs/heads/*:refs/remotes/origin/*'
expect_allow 'git -C . fetch origin'
# --tags and tag refspecs write local tags; --depth and friends rewrite .git/shallow; `..` walks a
# destination out of refs/remotes/.
expect_block 'git fetch --tags -q origin'
expect_block 'git fetch --tags origin'
expect_block 'git fetch -t origin'
expect_block 'git fetch -qt origin'
expect_block 'git fetch origin tag v1.0'
expect_block 'git fetch origin refs/tags/v1.0'
expect_block 'git fetch origin +refs/tags/*:refs/remotes/origin/tags/*'
expect_block 'git fetch --depth=1 origin main'
expect_block 'git fetch --deepen=5 origin'
expect_block 'git fetch --unshallow origin'
expect_block 'git fetch --shallow-since=2020-01-01 origin'
expect_block 'git fetch --update-shallow origin'
expect_block 'git fetch origin main:refs/remotes/../heads/main'
expect_block 'git fetch origin refs/heads/main:refs/remotes/origin/../../heads/x'
expect_block 'git fetch origin main..dev'
expect_block 'git fetch --upload-pack=x origin'
expect_block 'git fetch --upload-pack x origin'
expect_block 'git fetch --upload origin'
expect_block 'git fetch --receive-pack=x origin'
expect_block 'git fetch --update-head-ok origin main:main'
expect_block 'git fetch -u origin'
expect_block 'git fetch --prune origin'
expect_block 'git fetch --pru origin'
expect_block 'git fetch -p origin'
expect_block 'git fetch -pq origin'
expect_block 'git fetch --prune-tags origin'
expect_block 'git fetch --force origin main'
expect_block 'git fetch -f origin main'
expect_block 'git fetch --refmap= origin main'
expect_block 'git fetch --recurse-submodules origin'
expect_block 'git fetch --stdin origin'
expect_block 'git fetch -j 4 ../repo'
expect_block 'git fetch https://example.com/y main'
expect_block 'git fetch ../other main'
expect_block 'git fetch /tmp/repo'
expect_block 'git fetch git@example.com:o/r.git'
expect_block 'git fetch ext::sh origin'
expect_block 'git fetch origin main:main'
expect_block 'git fetch origin +main:refs/heads/main'
expect_block 'git fetch origin refs/heads/main:refs/heads/other'
expect_block 'git fetch origin :refs/remotes/origin/x'
expect_block 'git fetch origin main:HEAD'
expect_block 'git fetch $remote'
expect_block 'git fetch origin $ref'
expect_block 'git -c remote.origin.url=x fetch origin'
expect_block 'git -c remote.origin.fetch=+refs/*:refs/heads/* fetch origin'
expect_block 'bash -c "git fetch --upload-pack=x origin"'
expect_block 'bash -c "git fetch origin main:main"'
expect_allow 'bash -c "git fetch origin main"'

# A global-option value computed at run time blocks, except a double-quoted plain variable in -C or
# --work-tree. The subcommand, -c, --config-env, and unquoted values stay strict.
expect_allow 'git -C "$r" show HEAD:x'
expect_allow 'git -C "${r}" log'
expect_allow 'git -C "$r/sub" status'
expect_allow 'git --work-tree="$r" status'
expect_allow 'git --work-tree "$r" status'
expect_block 'git -C $r show'
expect_block 'git -C "$r"$d show'
expect_block 'git -C "$(echo x)" show'
expect_block 'git -C "`echo x`" show'
expect_block 'git -C "${r:-x}" show'
expect_block 'git -C "$r" commit -m x'
expect_block 'git -C "$r" fetch origin main:main'
expect_block 'git -c "$k" status'
expect_block 'git -c "core.pager=$p" status'
expect_block 'git --config-env="$k" status'
expect_block 'git --git-dir="$r" status'
expect_block 'git "$s"'
expect_block 'git -C "$r" "$s"'
expect_block 'git $s status'
expect_block 'aitools run -- git -C "$r" show'
expect_block 'jj util exec -- git -C "$r" show'
expect_allow 'bash -c "git -C \"$r\" show"'
expect_block 'git clone https://example.invalid/x.git'
expect_block 'git am x.patch'
expect_block 'git apply x.patch'
expect_block 'git gc'
expect_block 'git update-ref -d refs/heads/x'
expect_block 'git commit-tree HEAD^{tree}'
expect_block 'git init'
expect_block 'git bisect start'
expect_block 'git worktree add ../x'
expect_block 'git worktree remove x'
expect_block 'git remote add x url'
expect_block 'git remote remove x'
expect_block 'git remote set-url origin x'
expect_block 'git config user.name x'
expect_block 'git config --global user.name x'
expect_block 'git config --unset user.name'
expect_block 'git config --get user.name --edit'
expect_block 'git branch feat'
expect_block 'git branch -d feat'
expect_block 'git branch -D feat'
expect_block 'git branch -m old new'
expect_block 'git branch -f feat HEAD'
expect_block 'git branch --set-upstream-to=origin/main'
expect_block 'git tag v1'
expect_block 'git tag -a v1 -m x'
expect_block 'git tag -d v1'
expect_block 'git stash list; git stash pop'
expect_block 'git stash show'
# A read-only subcommand turned into a program launch or a file write.
expect_block 'git -c core.fsmonitor=evil status'
expect_block 'git -c core.pager=evil log'
expect_block 'git -c alias.x=!evil x'
expect_block 'git -ccore.pager=evil log'
expect_block 'git --config-env=core.pager=VAR log'
expect_block 'git --exec-path=/tmp/x status'
expect_block 'git diff --output=x'
expect_block 'git log --output x'
expect_block 'git diff --ext-diff'
expect_block 'git grep -O less foo'
expect_block 'GIT_EXTERNAL_DIFF=evil git diff'
expect_block 'GIT_PAGER=evil git log'
expect_block 'env GIT_SSH_COMMAND=evil git ls-remote origin'
expect_block 'git ls-remote --upload-pack=evil origin'
# git resolves a unique prefix of a long option, so an abbreviation runs the same program or writes the same file.
expect_block 'git diff --outp=/tmp/x'
expect_block 'git log -p --ext'
expect_block 'git grep --open-files=sh x'
expect_block 'git ls-remote --upl=sh .'
expect_block 'git ls-remote --exe=sh .'
expect_block 'git branch -r --del origin/x'
expect_block 'git branch -a --mov a b'
expect_block 'git branch -a --forc a b'
expect_block 'git diff --no-index a b'
expect_block 'git branch --unset-upstream'
expect_block 'git branch --delete x'
expect_block 'git branch -c a b'
expect_block 'git tag --delete v1'
# -O (pager, order file) hides in a short bundle.
expect_block 'git grep -nOsh x'
expect_block 'git grep -Oless x'
# Config keys that run a program.
expect_block 'git -c man.viewer=x help -m git'
expect_block 'git -c diff.x.textconv=id show'
expect_block 'git -c diff.x.command=id show'
expect_block 'git -c diff.external=id show'
expect_block 'git -c merge.x.driver=id log'
expect_block 'git -c core.alternateRefsCommand=x log'
expect_block 'git -c browser.x.cmd=x log'
expect_block 'git -c help.browser=x log'
expect_block 'git -c sequence.editor=x log'
expect_block 'git -c mergetool.x.cmd=x log'
expect_block 'git -c difftool.x.cmd=x log'
expect_block 'git -c http.proxy=x ls-remote origin'
expect_block 'git -c gpg.program=x log'
expect_block 'git -c credential.helper=x log'
# Environment that redirects config, repository, or helper programs.
expect_block 'GIT_CONFIG_GLOBAL=/x git status'
expect_block 'GIT_CONFIG_SYSTEM=/x git status'
expect_block 'GIT_CONFIG_COUNT=1 git status'
expect_block 'GIT_CONFIG_KEY_0=core.pager git status'
expect_block 'GIT_CONFIG_VALUE_0=x git status'
expect_block 'GIT_TRACE=/tmp/x git status'
expect_block 'GIT_TRACE2_EVENT=/tmp/x git status'
expect_block 'GIT_DIR=/x git status'
expect_block 'GIT_WORK_TREE=/x git status'
expect_block 'GIT_EXEC_PATH=/x git status'
expect_block 'GIT_SSH_COMMAND=x git ls-remote origin'
expect_block 'GIT_ASKPASS=x git status'
expect_block 'GIT_EDITOR=x git status'
expect_block 'env GIT_DIR=/x git status'
# These print credentials or launch a viewer.
expect_block 'git help -w'
expect_block 'git help commit'
expect_block 'git config --list'
expect_block 'git config -l'
expect_block 'git config --get-regexp .'
expect_block 'git remote update'
expect_block 'git remote prune origin'
# The subcommand or an option value the shell computes at run time cannot be proven read-only.
expect_block 'git $(echo status)'
expect_block 'git $SUB'
expect_block 'git -C $DIR status'
expect_block 'git -C $(echo x) status'
expect_block 'git log $(git commit -m x)'
# Wrappers and path forms still find the git underneath.
expect_block 'cd repo && git -C . switch main'
expect_block 'echo ok; git push'
expect_block '/usr/bin/git commit -m x'
expect_block 'bash -c "git push"'
expect_block "bash -c 'git push'"
expect_block 'bash -c "git stash"'
expect_block 'sudo git commit -m x'
expect_block 'env git commit -m x'
expect_block 'nohup git push'
expect_block 'xargs git rm'
expect_block 'eval git push'
expect_block 'command git push'
expect_block 'git -C x commit -m x'
expect_block 'git -C . -c color.ui=never push'
expect_block 'git --no-pager commit'
expect_block 'git --git-dir=x.git push'
expect_block 'aitools run -- git push'
expect_block 'aitools run -- git commit -m x'
expect_allow 'aitools bg start -- /usr/bin/git fetch origin'
expect_block 'aitools bg start -- /usr/bin/git fetch --prune'
expect_block 'aitools -q run git push'
# Reserved words and wrapper options that precede the command.
expect_block '! git push'
expect_block 'if true; then git push; fi'
expect_block 'for i in 1; do git commit -m x; done'
expect_block 'while true; do git add .; done'
expect_block '{ git push; }'
expect_block 'env -u X git push'
expect_block 'env --unset X git push'
expect_block 'env -C . git push'
expect_block 'sudo -u x git push'
expect_block 'sudo --user=x git push'
expect_block 'sudo --user x git push'
expect_block 'sudo -u x -g y git push'
expect_block '! jj undo'
expect_block 'if true; then jj undo; fi'
# command -v/-V only print a path.
expect_allow 'command -v git'
expect_allow 'command -V git'
# The override is for the jj verdicts; it never unblocks a git write.
expect_block 'ALLOW_DESTRUCTIVE_GIT=1 git push'
expect_block 'CLAUDE_ALLOW_DESTRUCTIVE_GIT=1 git commit -m x'
expect_block 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon; git push'
expect_block 'ALLOW_DESTRUCTIVE_GIT=1 bash -c "git push"'
expect_block 'bash -c "ALLOW_DESTRUCTIVE_GIT=1 git push"'
expect_allow 'echo git'
expect_allow 'rg -n "git push" docs'
expect_allow 'gitleaks git --config x'
expect_allow 'aitools read git'
expect_allow 'aitools search git src'
expect_allow $'cat <<EOF\ngit push\nEOF'

# jj: commands that rewrite the shared working copy.
expect_block 'jj edit abc'
expect_block 'jj next'
expect_block 'jj prev --edit'
expect_block 'jj abandon'
expect_block 'jj abandon xyz'
expect_block 'jj undo'
expect_block 'jj bisect run --range a..b -- true'
expect_block 'jj sparse set --add x'
expect_block 'jj sparse reset'
expect_block 'jj sparse edit'
expect_block 'jj sparse $(echo reset)'
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
expect_allow 'jj sparse list'
expect_allow 'jj bisect --help'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj sparse reset'
expect_allow 'jj edit --help'
expect_allow 'jj log -r @'
expect_allow 'jj status'
expect_allow 'jj describe -m "feat: x"'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon'
expect_allow 'bash -c "ALLOW_DESTRUCTIVE_GIT=1 jj abandon"'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 bash -c "jj abandon"'

# jj git push skips the gitleaks pre-commit hook, so it needs the override.
expect_block 'jj git push'
expect_block 'jj git push -b main'
expect_block 'jj -R . git push --all'
expect_allow 'jj git push --dry-run'
expect_allow 'jj git fetch'
expect_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'

# A push verdict never hides a later segment's verdict.
expect_block 'jj git push -b x; jj undo'
expect_block 'jj git push -b x && git commit -am z'
expect_block 'jj git push -b x || jj abandon'
expect_block 'bash -c "jj git push -b x; jj undo"'
expect_block 'jj git push -b x; git push'
expect_block 'jj git push -b x; jj git push -b y'

# `claude` mode turns the jj push verdict into an ask decision on stdout (exit 0); every other verdict
# still exits 2. Without the argument the verdict is the exit-2 banner tested above.
expect_ask() {
  checked=$((checked + 1))
  local status=0 out
  out=$(jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' | bash "$hook" claude 2>"$stderr_file") || status=$?
  [[ $status -eq 0 ]] || fail "claude mode: expected exit 0 for [$1], got exit $status"
  [[ $(jq -r '.hookSpecificOutput.permissionDecision' <<<"$out") == ask ]] || fail "claude mode: no ask decision for [$1]: $out"
  [[ $(jq -r '.hookSpecificOutput.hookEventName' <<<"$out") == PreToolUse ]] || fail "claude mode: wrong event name for [$1]: $out"
  [[ $(jq -r '.hookSpecificOutput.permissionDecisionReason' <<<"$out") == *gitleaks* ]] || fail "claude mode: reason lacks gitleaks instructions for [$1]"
  [[ $(jq -r '.hookSpecificOutput.permissionDecisionReason' <<<"$out") != *ALLOW_DESTRUCTIVE_GIT* ]] || fail "claude mode: reason still mentions the override for [$1]"
  [[ ! -s $stderr_file ]] || fail "claude mode: ask for [$1] wrote to stderr"
}
expect_claude_block() {
  checked=$((checked + 1))
  local status=0 out
  out=$(jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' | bash "$hook" claude 2>"$stderr_file") || status=$?
  [[ $status -eq 2 ]] || fail "claude mode: expected block (exit 2) for [$1], got exit $status"
  [[ -z $out ]] || fail "claude mode: block for [$1] printed to stdout: $out"
  grep -q 'ORCH-P005' "$stderr_file" || fail "claude mode: block for [$1] printed no ORCH-P005 banner"
}
expect_claude_allow() {
  checked=$((checked + 1))
  local status=0 out
  out=$(jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' | bash "$hook" claude 2>"$stderr_file") || status=$?
  [[ $status -eq 0 && -z $out && ! -s $stderr_file ]] || fail "claude mode: expected silent allow for [$1], got exit $status, stdout [$out]"
}
expect_ask 'jj git push -b main'
expect_ask 'jj -R . git push --all'
expect_ask 'jj git push -b $(jj log -r @- -T bookmarks)'
expect_claude_allow 'jj git push --dry-run'
# The override never lifts a push in claude mode, wherever it appears; the person always confirms.
expect_ask 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'
expect_ask 'CLAUDE_ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'
expect_ask 'FOO=1 ALLOW_DESTRUCTIVE_GIT=1 jj git push'
expect_ask 'ALLOW_DESTRUCTIVE_GIT=1 FOO=1 jj git push'
expect_ask 'bash -c "ALLOW_DESTRUCTIVE_GIT=1 jj git push"'
expect_ask 'ALLOW_DESTRUCTIVE_GIT=1 bash -c "jj git push"'
expect_ask 'env ALLOW_DESTRUCTIVE_GIT=1 jj git push'
expect_ask 'ALLOW_DESTRUCTIVE_GIT=1 jj util exec -- jj git push'
# Other jj verdicts are still lifted by the override, so only the push remains.
expect_ask 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon; jj git push -b x'
expect_claude_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon'
# A push in the same command line does not hide another block, with or without the override.
expect_claude_block 'jj git push -b x; jj undo'
expect_claude_block 'jj git push -b x && git commit -am z'
expect_claude_block 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b x; git commit -am z'
expect_claude_block 'jj git push --allow-private -b main'
expect_claude_block 'jj --config user.email=x@example.com git push -b main'
expect_claude_block 'git push'
expect_claude_block 'jj abandon'
expect_claude_block 'eval eval eval eval eval git push'
# Default mode keeps the banner and never prints a decision.
checked=$((checked + 1))
out=$(jq -n '{tool_name: "Bash", tool_input: {command: "jj git push -b main"}}' | bash "$hook" 2>/dev/null) || true
[[ -z $out ]] || fail "default mode printed to stdout: $out"

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

# Wrappers whose operands are not the command.
expect_block 'exec -a name jj abandon'
expect_block 'exec -a name git push'
expect_block 'env -S "jj abandon"'
expect_block "env -S'git push'"
expect_allow 'jj'
expect_allow 'jj --version'

# Nesting past the recursion limit still runs the innermost command.
expect_block 'eval eval eval eval eval jj abandon'
expect_block 'eval eval eval eval eval echo ok'
expect_allow 'eval eval eval eval jj log'
expect_block 'eval eval eval eval jj abandon'
expect_block 'eval eval eval eval git push'
nested='ls'
for _ in 1 2 3 4; do nested="bash -c $(printf '%q' "$nested")"; done
expect_allow "$nested"
for _ in 5 6; do nested="bash -c $(printf '%q' "$nested")"; done
expect_block "$nested"

# jj util exec runs its operand as a command.
expect_block 'jj util exec -- git push'
expect_block 'jj util exec -- git stash'
expect_block 'jj util exec -- bash -c "jj abandon"'

# Data, not invocations.
expect_allow 'echo "jj abandon"'
expect_allow "rg -n 'jj undo' docs"
expect_allow $'cat <<EOF\njj abandon\nEOF'

# Nothing to classify: malformed input and an empty command stand aside.
checked=$((checked + 1))
status=0
printf 'not json' | bash "$hook" >/dev/null 2>&1 || status=$?
[[ $status -eq 0 ]] || fail "malformed JSON: expected exit 0, got $status"
expect_allow ''

# Other tools stand aside.
checked=$((checked + 1))
status=0
jq -n '{tool_name: "Read", tool_input: {command: "jj abandon"}}' | bash "$hook" >/dev/null 2>&1 || status=$?
[[ $status -eq 0 ]] || fail "non-Bash tool: expected exit 0, got $status"

echo "block-destructive-git: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
