#!/bin/bash
# Table-driven test for block-destructive-git.sh, run by the `block-destructive-git-hook` flake check.
# Usage: block-destructive-git.test.sh <path-to-block-destructive-git.sh>

set -euo pipefail

hook="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

stderr_file=$(mktemp)
push_fixture=''
cleanup() {
  rm -f "$stderr_file"
  if [[ -n $push_fixture ]]; then rm -rf "$push_fixture"; fi
}
trap cleanup EXIT

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

expect_block_message() {
  checked=$((checked + 1))
  local status=0 out stderr
  out=$(jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' | bash "$hook" "$3" 2>"$stderr_file") || status=$?
  stderr=$(cat "$stderr_file")
  [[ $status -eq 2 ]] || fail "$3 mode: expected block (exit 2) for [$1], got exit $status"
  [[ -z $out ]] || fail "$3 mode: block for [$1] printed to stdout: $out"
  [[ $stderr == *ORCH-P005* ]] || fail "$3 mode: block for [$1] printed no ORCH-P005 banner"
  [[ $stderr == *"$2"* ]] || fail "$3 mode: block for [$1] omitted expected guidance: $2"
}

expect_runtime_allow() {
  checked=$((checked + 1))
  local status=0 out
  out=$(jq -n --arg c "$1" '{tool_name: "Bash", tool_input: {command: $c}}' |
    bash "$hook" "$2" 2>"$stderr_file") || status=$?
  [[ $status -eq 0 && -z $out && ! -s $stderr_file ]] ||
    fail "$2 mode: expected silent allow for [$1], got exit $status"
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
expect_block 'git -C $r log'
expect_block 'git --work-tree $r status'
expect_allow 'git -C "$dir" log'
expect_allow 'git --work-tree="$dir" status'
git_dynamic_path_guidance=$'❌ Unquoted dynamic git path blocked (ORCH-P005)\n\nQuote the variable expansion so the shell passes the repository path as one argument:\n  git -C "$dir" log\n  git --work-tree="$dir" status'
expect_block_message 'git -C $r log' "$git_dynamic_path_guidance" codex
expect_block_message 'git --work-tree $r status' "$git_dynamic_path_guidance" claude
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
# Global option values must not be mistaken for launcher subcommands.
expect_block 'aitools --root run run -- git push --force'
expect_block 'aitools --root run run -- jj git push -b x'
expect_block 'aitools --root run bg start -- git push --force'
expect_block 'aitools --root run run --cwd x -- git reset --hard'
expect_block 'aitools --root=run run -- git push --force'
expect_block 'aitools --root bg run -- git push --force'
expect_block 'aitools --root git run -- git push --force'
expect_block 'aitools --root bash run -- git push --force'
expect_block 'aitools --lock-timeout 1s --root run run -- git push --force'
expect_block 'aitools --trace --root run run -- git push --force'
expect_block 'aitools --bogus run run -- git push --force'
# An option value named gh is not the launched program.
expect_block 'aitools run --cwd home-manager/vcs/gh -- git reset --hard'
expect_block 'aitools run --cwd ../gh -- jj git push --bookmark x'
expect_block 'aitools run --grep gh -- git push --force origin main'
expect_block 'aitools run --stdout-to /tmp/gh -- sh -c "git reset --hard"'
# Nor is an option value named jj or a shell.
expect_block 'aitools run --grep bash -- git clean -fdx'
expect_block 'aitools run --grep jj -- git reset --hard'
expect_block 'aitools run --cwd jj -- git reset --hard'
expect_block 'aitools run --cwd sh -- git reset --hard'
expect_block 'aitools run --cwd /x/jj -- git branch -D main'
expect_block 'aitools run --stdout-to out/sh -- jj git push --bookmark x'
# An unknown option leaves the program position undetermined; every candidate is still classified.
expect_block 'aitools run --bogus jj -- git reset --hard'
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
expect_block 'jj workspace add --name w /p'
workspace_add_guidance=$'❌ jj workspace add blocked (ORCH-P005)\n\nA workspace made without sparse patterns inherits the empty sparse set from the bare repository and checks out no files.\nUse this complete form:\n  jj -R <repo> workspace add --no-colocate --sparse-patterns full --name <dir> -r <rev> <repo>/.worktrees/<dir>'
expect_block_message 'jj workspace add --name w /p' "$workspace_add_guidance" codex
expect_allow 'jj workspace add --sparse-patterns full --name w -r @ /repo/.worktrees/w'
expect_allow 'jj workspace add --name w -r @ --sparse-patterns=full /repo/.worktrees/w'
expect_block 'jj --ignore-working-copy -R . workspace add --name w /p'

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

# `claude` mode allows the jj push verdict silently (exit 0); every other verdict still exits 2.
# Without the argument the verdict is the exit-2 banner tested above.
# The Claude push cases scan a real, isolated no-colocate repository.
# Only the clean baseline is pushed, to a local bare receiver. Secret revisions never leave it.
push_fixture=$(mktemp -d /tmp/block-destructive-git.XXXXXX)
real_jj=$(command -v jj)
real_gitleaks=$(command -v gitleaks)
export GITLEAKS_BIN="$real_gitleaks"
export JJ_CONFIG="$push_fixture/jj.toml"
cat >"$JJ_CONFIG" <<'CONFIG'
[user]
name = "Hook Test"
email = "hook-test@example.invalid"
[git]
push = "origin"
[signing]
behavior = "drop"
CONFIG
fixture_jj() {
  (cd "$push_fixture/sender" && "$real_jj" "$@") >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
}
"$real_jj" git init --no-colocate "$push_fixture/receiver" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
"$real_jj" git init --no-colocate "$push_fixture/sender" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
printf 'clean baseline\n' >"$push_fixture/sender/clean.txt"
fixture_jj commit clean.txt -m 'Clean baseline'
fixture_jj bookmark create main -r @-
fixture_jj git remote add origin "$push_fixture/receiver/.jj/repo/store/git"
fixture_jj git push --remote origin --bookmark main
printf 'clean candidate\n' >>"$push_fixture/sender/clean.txt"
fixture_jj commit clean.txt -m 'Clean candidate'
fixture_jj bookmark set main -r @-
fixture_jj bookmark create x -r @-
push_cwd="$push_fixture/sender"
invoke_claude_hook() {
  (
    cd "$push_cwd"
    jq -n --arg c "$1" --arg cwd "$push_cwd" \
      '{tool_name: "Bash", tool_input: {command: $c}, cwd: $cwd}' | bash "$hook" claude
  )
}

expect_claude_block() {
  checked=$((checked + 1))
  local status=0 out
  out=$(invoke_claude_hook "$1" 2>"$stderr_file") || status=$?
  [[ $status -eq 2 ]] || fail "claude mode: expected block (exit 2) for [$1], got exit $status"
  [[ -z $out ]] || fail "claude mode: block for [$1] printed to stdout: $out"
  grep -q 'ORCH-P005' "$stderr_file" || fail "claude mode: block for [$1] printed no ORCH-P005 banner"
}
expect_claude_allow() {
  checked=$((checked + 1))
  local status=0 out
  out=$(invoke_claude_hook "$1" 2>"$stderr_file") || status=$?
  [[ $status -eq 0 && -z $out && ! -s $stderr_file ]] || fail "claude mode: expected silent allow for [$1], got exit $status, stdout [$out]"
}
expect_claude_allow 'jj git push -b main'
expect_claude_allow 'jj -R . git push --all'
expect_claude_block 'jj git push -b $(jj log -r @- -T bookmarks)'
expect_claude_allow 'jj git push --dry-run'
# A push is allowed in claude mode whether or not the override appears.
expect_claude_allow 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'
expect_claude_allow 'CLAUDE_ALLOW_DESTRUCTIVE_GIT=1 jj git push -b main'
expect_claude_block 'FOO=1 ALLOW_DESTRUCTIVE_GIT=1 jj git push'
expect_claude_block 'ALLOW_DESTRUCTIVE_GIT=1 FOO=1 jj git push'
# FR-002 requires wrapped pushes to be rerun directly, including overrides.
expect_claude_block 'bash -c "ALLOW_DESTRUCTIVE_GIT=1 jj git push"'
expect_claude_block 'ALLOW_DESTRUCTIVE_GIT=1 bash -c "jj git push"'
expect_claude_block 'env ALLOW_DESTRUCTIVE_GIT=1 jj git push'
expect_claude_block 'ALLOW_DESTRUCTIVE_GIT=1 jj util exec -- jj git push'
# A compound command can change the revisions after inspection, so rerun the push directly.
expect_claude_block 'ALLOW_DESTRUCTIVE_GIT=1 jj abandon; jj git push -b x'
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

# Batch inspection includes words in the producer segment without interpreting JSON.
for runtime in claude codex; do
  expect_block_message "echo '[[\"run\",\"--\",\"git\",\"push\",\"--force\"]]' | aitools batch --stdin" 'ORCH-P005' "$runtime"
  expect_block_message "printf '%s\\n' '[[\"run\",\"--\",\"git\",\"reset\",\"--hard\"]]' | aitools batch --stdin" 'ORCH-P005' "$runtime"
  expect_block_message "echo '[[\"run\",\"--\",\"jj\",\"git\",\"push\"]]' | aitools batch --stdin" 'ORCH-P005' "$runtime"
  expect_block_message "printf '%s\\n' '[[\"run\",\"--\",\"jj\",\"abandon\"]]' | aitools batch --stdin" 'ORCH-P005' "$runtime"
  expect_runtime_allow "echo '[[\"run\",\"--\",\"/bin/echo\",\"batch-ok\"]]' | aitools batch --stdin" "$runtime"
  expect_runtime_allow "printf '%s\\n' '[[\"run\",\"--\",\"git\",\"status\"]]' | aitools batch --stdin" "$runtime"
  expect_runtime_allow 'aitools --root batch run -- echo git push --force' "$runtime"
  expect_runtime_allow 'echo "aitools batch git push --force"' "$runtime"
  expect_runtime_allow 'aitools search batch .; echo git push --force' "$runtime"
done

# GitHub policy belongs to the argv-level gh wrapper; git/jj remain this hook's responsibility.
for runtime in claude codex; do
  expect_runtime_allow 'gh pr merge 12' "$runtime"
  expect_runtime_allow 'aitools run -- gh api -X DELETE repos/example/project' "$runtime"
  expect_runtime_allow 'aitools read gh' "$runtime"
  expect_runtime_allow 'echo "gh pr merge 12"' "$runtime"
  expect_runtime_allow 'aitools run -- gh issue create --title jj --body x' "$runtime"
  expect_runtime_allow 'aitools run -- gh search issues git push' "$runtime"
  expect_runtime_allow 'aitools bg start -- /usr/bin/gh search issues git push' "$runtime"
  expect_runtime_allow 'aitools run --cwd ../x gh search issues git push' "$runtime"
  expect_runtime_allow 'aitools run --trace -- gh search issues git push --force' "$runtime"
  expect_runtime_allow 'aitools run --lock-timeout 1s -- gh search issues git push --force' "$runtime"
  expect_block_message 'git commit src/app.sh -m update' 'jj commit <paths> -m' "$runtime"
  expect_block_message 'jj git push -b x; gh pr merge 12' 'ORCH-P005' "$runtime"
done
expect_block 'bash -cl "jj git push"'

# Every refusal assertion checks a nonempty stderr banner as well as exit 2.
for invocation in \
  'bash -c "jj git push"' \
  'bash -cl "jj git push"' \
  'xargs -I {} jj git push -b {}' \
  'zsh -lc "jj git push -b feature"' \
  'env jj git push' \
  'env -S "jj git push"' \
  'command jj git push' \
  'sudo jj git push' \
  'eval jj git push' \
  'jj util exec -- jj git push' \
  'aitools run -- jj git push' \
  'aitools run -- bash -c "jj git push"' \
  'ALLOW_DESTRUCTIVE_GIT=1 aitools run -- jj git push'; do
  expect_block_message "$invocation" 'Run jj git push directly' claude
done

# Invalid input must not become a silent success when the safety parser fails.
checked=$((checked + 1))
status=0
printf 'not json' | bash "$hook" >/dev/null 2>"$stderr_file" || status=$?
[[ $status -eq 2 && -s $stderr_file ]] || fail "malformed JSON: expected refusal, got $status"
expect_allow ''
for runtime in claude codex; do
  for bad_input in 'not json' '[]' '{"tool_name":"Bash","tool_input":{"command":7}}' \
    '{"tool_name":"Bash","tool_input":null}'; do
    checked=$((checked + 1))
    status=0
    out=$(printf '%s' "$bad_input" | bash "$hook" "$runtime" 2>"$stderr_file") || status=$?
    [[ $status -eq 2 && -z $out && -s $stderr_file ]] || fail "$runtime malformed event: expected refusal, got $status"
  done
done
bash_bin=$(command -v bash)
# Test each real parser in isolation: a second JSON value must not turn tool_name into
# a multi-line non-Bash name and bypass classification.
for parser in jq python3; do
  parser_path="$push_fixture/parser-$parser"
  mkdir "$parser_path"
  for dependency in cat perl "$parser"; do
    ln -s "$(command -v "$dependency")" "$parser_path/$dependency"
  done
  for runtime in claude codex; do
    for bad_input in '' 'null' '[]' '{} {}' \
      '{"tool_name":"Bash","tool_input":{"command":"git status"}} {"tool_name":"Bash","tool_input":{"command":"git push"}}' \
      '{"tool_name":"Bash","tool_input":{"command":"git push"}} {"tool_name":"Read"}' \
      '{"tool_name":"Read"} {"tool_name":"Bash","tool_input":{"command":"git push"}}' \
      '{"tool_name":"Bash","tool_input":{"command":"git status"}} null' \
      'null {"tool_name":"Bash","tool_input":{"command":"git status"}}' \
      '{"tool_name":"Bash","tool_input":{"command":"git status"}} trailing'; do
      checked=$((checked + 1))
      status=0
      out=$(printf '%s' "$bad_input" | PATH="$parser_path" "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
      [[ $status -eq 2 && -z $out ]] || fail "$parser/$runtime invalid stream: expected exit 2 and no stdout, got $status"
      grep -Fq 'Hook input blocked (ORCH-P005)' "$stderr_file" || fail "$parser/$runtime invalid stream: missing input refusal"
    done
    for good_input in '{"tool_name":"Bash","tool_input":{"command":"git status"}}' \
      '{"tool_name":"Bash","tool_input":{"command":""}}' '{"tool_name":"Read"}'; do
      checked=$((checked + 1))
      status=0
      out=$(printf '%s' "$good_input" | PATH="$parser_path" "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
      [[ $status -eq 0 && -z $out && ! -s $stderr_file ]] || fail "$parser/$runtime single event: expected silent allow, got $status"
    done
    checked=$((checked + 1))
    status=0
    out=$(printf '%s' '{"tool_name":"Bash","tool_input":{"command":"git push"}}' |
      PATH="$parser_path" "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
    [[ $status -eq 2 && -z $out ]] || fail "$parser/$runtime single destructive event: expected exit 2, got $status"
    grep -Fq 'ORCH-P005' "$stderr_file" || fail "$parser/$runtime single destructive event: missing refusal"
  done
done
mkdir "$push_fixture/no-parsers"
ln -s "$(command -v cat)" "$push_fixture/no-parsers/cat"
for runtime in claude codex; do
  checked=$((checked + 1))
  status=0
  out=$(printf '%s' '{"tool_name":"Bash","tool_input":{"command":"jj git push"}}' |
    PATH="$push_fixture/no-parsers" "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
  [[ $status -eq 2 && -z $out && -s $stderr_file ]] || fail "$runtime missing parsers: expected refusal, got $status"
done

# Classifier startup failures must use the hook's refusal protocol too.
mkdir "$push_fixture/no-perl"
ln -s "$(command -v cat)" "$push_fixture/no-perl/cat"
ln -s "$(command -v jq)" "$push_fixture/no-perl/jq"
for runtime in claude codex; do
  checked=$((checked + 1))
  status=0
  out=$(printf '%s' '{"tool_name":"Bash","tool_input":{"command":"git push"}}' |
    PATH="$push_fixture/no-perl" "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
  [[ $status -eq 2 && -z $out ]] || fail "$runtime missing perl: expected exit 2 with no stdout, got $status"
  grep -Fq 'Command classification failed (ORCH-P005).' "$stderr_file" ||
    fail "$runtime missing perl: missing classifier refusal reason"

  checked=$((checked + 1))
  status=0
  out=$(jq -nc '{tool_name:"Bash",tool_input:{command:("echo " + ("x" * 1100000))}}' |
    "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
  [[ $status -eq 2 && -z $out ]] || fail "$runtime oversized classifier argv: expected exit 2 with no stdout, got $status"
  grep -Fq 'Command classification failed (ORCH-P005).' "$stderr_file" ||
    fail "$runtime oversized classifier argv: missing classifier refusal reason"
done

# Perl diagnostics must survive both successful classification and startup failure.
mkdir "$push_fixture/diagnostic-perl"
real_perl=$(command -v perl)
export real_perl
cat >"$push_fixture/diagnostic-perl/perl" <<'PERL_STUB'
#!/bin/bash
printf 'R11 classifier diagnostic\n' >&2
if [[ ${CLASSIFIER_PROBE_FAILURE:-0} == 1 ]]; then exit 17; fi
exec "$real_perl" "$@"
PERL_STUB
chmod +x "$push_fixture/diagnostic-perl/perl"
for runtime in claude codex; do
  for probe_failure in 0 1; do
    checked=$((checked + 1))
    status=0
    out=$(printf '%s' '{"tool_name":"Bash","tool_input":{"command":"git status"}}' |
      PATH="$push_fixture/diagnostic-perl:$PATH" CLASSIFIER_PROBE_FAILURE="$probe_failure" \
      "$bash_bin" "$hook" "$runtime" 2>"$stderr_file") || status=$?
    expected_status=$((probe_failure * 2))
    [[ $status -eq $expected_status && -z $out ]] || fail "$runtime classifier diagnostic: unexpected status $status or stdout"
    grep -Fq 'R11 classifier diagnostic' "$stderr_file" || fail "$runtime classifier diagnostic was lost"
    if ((probe_failure)); then
      grep -Fq 'Command classification failed (ORCH-P005).' "$stderr_file" ||
        fail "$runtime classifier failure omitted refusal reason"
    fi
  done
done

# Other tools stand aside.
checked=$((checked + 1))
status=0
jq -n '{tool_name: "Read", tool_input: {command: "jj abandon"}}' | bash "$hook" >/dev/null 2>&1 || status=$?
[[ $status -eq 0 ]] || fail "non-Bash tool: expected exit 0, got $status"

# Exercise range selection against real jj history and real gitleaks.
fixture_jj tag set clean -r x
expect_claude_allow 'jj git push --tag clean'
for invocation in \
  'jj git push' \
  'jj git push -b main' \
  'jj git push --bookmark=main' \
  'jj git push -b main -b x' \
  'jj git push --remote origin -b main' \
  'jj git push -b main --remote=origin' \
  'jj git push -c @-' \
  'jj git push -r main' \
  'jj git push --all' \
  'jj git push --tracked'; do
  expect_claude_allow "$invocation"
done

# A fresh remote has no base: new bookmarks must scan their full reachable history.
"$real_jj" git init --no-colocate "$push_fixture/empty-receiver" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
fixture_jj git remote add empty "$push_fixture/empty-receiver/.jj/repo/store/git"
expect_claude_allow 'jj git push --remote empty -b main'
expect_claude_allow 'jj git push --remote origin --remote empty -b main'

# A snapshotting push may not target the mutable working-copy commit.
fixture_jj bookmark create working -r @
expect_claude_block 'jj git push -c @'
expect_claude_block 'jj git push -b working'
expect_claude_block 'jj git push -r @'
expect_claude_allow 'jj --ignore-working-copy git push -c @'
expect_claude_allow 'jj git push --ignore-working-copy -b working'
fixture_jj bookmark delete working

# A descendant is rewritten too when a later push snapshots its working-copy ancestor.
fixture_jj new --no-edit @ -m 'Working-copy child'
fixture_jj bookmark create working-child -r '@+'
expect_claude_block 'jj git push -b working-child'
grep -Fq 'jj commit <paths>' "$stderr_file" || fail 'working-copy child refusal omitted commit guidance'
expect_claude_allow 'jj --ignore-working-copy git push -b working-child'
fixture_jj bookmark delete working-child

# Remote knowledge does not prevent a later snapshot from rewriting @ and its descendants.
"$real_jj" git init --no-colocate "$push_fixture/known-sender" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
"$real_jj" git init --no-colocate "$push_fixture/known-receiver" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
(
  cd "$push_fixture/known-sender"
  printf 'clean known working copy\n' >clean.txt
  "$real_jj" describe -m 'Known working copy'
  "$real_jj" bookmark create known -r @
  "$real_jj" git remote add origin "$push_fixture/known-receiver/.jj/repo/store/git"
  "$real_jj" git push --ignore-working-copy --remote origin -b known
  known_parent=$("$real_jj" log --ignore-working-copy -r '@ & known@origin' --no-graph -T commit_id)
  [[ -n $known_parent ]] || exit 1
  "$real_jj" new --no-edit @ -m 'Known working-copy child'
  "$real_jj" bookmark create known-child -r '@+'
) >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
push_cwd="$push_fixture/known-sender"
expect_claude_block 'jj git push -b known-child'
grep -Fq 'jj commit <paths>' "$stderr_file" || fail 'known working-copy ancestor refusal omitted commit guidance'
expect_claude_allow 'jj --ignore-working-copy git push -b known-child'
push_cwd="$push_fixture/sender"

# Deleted and absent tracked references are empty targets, not conflicts.
fixture_jj bookmark create gone -r main@origin
fixture_jj git push --remote origin --bookmark gone
fixture_jj bookmark delete gone
expect_claude_allow 'jj git push --deleted'
expect_claude_allow 'jj git push -b main'
fixture_jj bookmark track x --remote empty
expect_claude_allow 'jj git push --remote empty -b x'
expect_claude_allow 'jj git push --remote origin -b main'

# Concurrent bookmark moves create a real conflict without changing file contents.
fixture_jj new main@origin -m 'Unrelated branch workspace'
printf 'other clean branch\n' >"$push_fixture/sender/other.txt"
fixture_jj commit other.txt -m 'Unrelated clean branch'
other_commit=$("$real_jj" --repository "$push_fixture/sender" log --ignore-working-copy -r @- --no-graph -T commit_id)
fixture_jj new x -m 'Return to clean branch'
fixture_jj bookmark create conflicted -r main@origin
conflict_base=$("$real_jj" --repository "$push_fixture/sender" op log --ignore-working-copy --limit 1 --no-graph -T id)
fixture_jj --at-operation "$conflict_base" bookmark set conflicted -r x
fixture_jj --at-operation "$conflict_base" bookmark set conflicted -r "$other_commit"
fixture_jj bookmark list
expect_claude_allow 'jj git push -b main'
expect_claude_block 'jj git push -b conflicted'
fixture_jj bookmark delete conflicted

# Different remote bases must select different outgoing histories.
cat >"$push_fixture/record-gitleaks" <<'SCANNER'
#!/usr/bin/env bash
report=''
while (($#)); do
  case "$1" in
    --report-path) report=$2; shift ;;
    --report-path=*) report=${1#*=} ;;
  esac
  shift
done
cat >/dev/null
printf 'scan\n' >>"$SCANNER_LOG"
printf '[]\n' >"$report"
SCANNER
chmod +x "$push_fixture/record-gitleaks"
check_scan_count() {
  local want=$1 command=$2 got
  : >"$push_fixture/scans"
  GITLEAKS_BIN="$push_fixture/record-gitleaks" SCANNER_LOG="$push_fixture/scans" \
    expect_claude_allow "$command"
  got=$(wc -l <"$push_fixture/scans")
  [[ $got -eq $want ]] || fail "outgoing history: expected $want scans, got $got"
}
check_scan_count 1 'jj git push --remote origin -b main'
check_scan_count 2 'jj git push --remote empty -b main'
check_scan_count 1 'jj git push --remote origin -b main -b x'
fixture_jj config set --repo git.push empty
check_scan_count 2 'jj git push -b main'
check_scan_count 1 'jj git push --remote origin -b main'
fixture_jj config set --repo git.push origin

synthetic_pat=$(perl -e '@a=("a".."z","A".."Z",0..9); print "ghp_", map {$a[int rand @a]} 1..36')
# A payload line starting with ++ must not be mistaken for a +++ diff filename.
printf '++ S1A_PRIVATE_VALUE_SENTINEL\nfixture_value=%s\n' "$synthetic_pat" >"$push_fixture/sender/fixture.txt"
fixture_jj commit fixture.txt -m 'Synthetic scanner positive control'
fixture_jj bookmark create leak -r @-
secret_commit=$("$real_jj" --repository "$push_fixture/sender" log --ignore-working-copy -r leak --no-graph -T commit_id)
fixture_jj tag set secret -r leak
expect_secret_block() {
  checked=$((checked + 1))
  local status=0 expected_commit=${2:-$secret_commit} expected_file=${3:-fixture.txt} expected_line=${4:-2}
  invoke_claude_hook "$1" >"$push_fixture/hook.stdout" 2>"$stderr_file" || status=$?
  [[ $status -eq 2 ]] || fail "secret history: expected exit 2, got $status"
  [[ ! -s $push_fixture/hook.stdout ]] || fail 'secret history: stdout was not empty'
  grep -q ORCH-P005 "$stderr_file" || fail 'secret history: missing refusal banner'
  grep -q github-pat "$stderr_file" || fail 'secret history: missing rule name'
  grep -Fq "$expected_file" "$stderr_file" || fail 'secret history: missing file name'
  grep -Eq "\"line\": *$expected_line([,}])" "$stderr_file" || fail 'secret history: missing source line'
  grep -q "$expected_commit" "$stderr_file" || fail 'secret history: missing commit id'
  if grep -Fq "$synthetic_pat" "$stderr_file" "$push_fixture/hook.stdout"; then
    fail 'secret history: raw synthetic value leaked'
  fi
  if grep -q S1A_PRIVATE_VALUE_SENTINEL "$stderr_file" "$push_fixture/hook.stdout"; then
    fail 'secret history: diff payload was leaked as a location'
  fi
}
expect_secret_block 'jj git push -b leak'
expect_secret_block 'jj git push --tag secret'
expect_secret_block 'ALLOW_DESTRUCTIVE_GIT=1 jj git push -b leak'
expect_secret_block 'jj git push --remote empty -b leak'
expect_secret_block 'jj git push -b main -b leak'
expect_secret_block 'jj git push --remote origin --remote empty -b leak'
expect_secret_block 'jj git push -c x -c leak'
expect_secret_block 'jj git push -r x -r leak'
expect_secret_block 'jj git push --remote "*" -b leak'
expect_secret_block 'jj git push -b "l*"'

# The endpoint is clean, but the secret existed in an intermediate outgoing commit.
printf 'removed synthetic value\n' >"$push_fixture/sender/fixture.txt"
fixture_jj commit fixture.txt -m 'Remove synthetic value'
fixture_jj bookmark set main -r @-
for invocation in \
  'jj git push' \
  'jj git push -b main' \
  'jj git push --bookmark=main' \
  'jj git push -c @-' \
  'jj git push -r main' \
  'jj git push --all' \
  'jj git push --tracked' \
  'jj git push --remote empty -b main'; do
  expect_secret_block "$invocation"
done
# Selecting an older clean bookmark must not scan unrelated descendants.
expect_claude_allow 'jj git push -b x'

# Scanner launch/exit/report failures must not become an empty successful scan.
cat >"$push_fixture/fake-gitleaks" <<'SCANNER'
#!/usr/bin/env bash
report=''
while (($#)); do
  case "$1" in
    --report-path) report=$2; shift ;;
    --report-path=*) report=${1#*=} ;;
  esac
  shift
done
cat >/dev/null
printf 'DO_NOT_ECHO_SCANNER_OUTPUT\n'
printf 'DO_NOT_ECHO_SCANNER_OUTPUT\n' >&2
case "$SCANNER_CASE" in
  missing) exit 127 ;;
  failure) exit 2 ;;
  malformed) printf 'not-json\n' >"$report"; exit 0 ;;
  no-report) exit 0 ;;
  empty-finding) printf '[]\n' >"$report"; exit 1 ;;
  non-object) printf '[1]\n' >"$report"; exit 1 ;;
esac
SCANNER
chmod +x "$push_fixture/fake-gitleaks"
for scanner_case in missing failure malformed no-report empty-finding non-object; do
  GITLEAKS_BIN="$push_fixture/fake-gitleaks" SCANNER_CASE="$scanner_case" \
    expect_claude_block 'jj git push -b x'
  if grep -q DO_NOT_ECHO_SCANNER_OUTPUT "$stderr_file"; then
    fail "scanner $scanner_case: raw scanner output leaked"
  fi
done
GITLEAKS_BIN="$push_fixture/not-installed" expect_claude_block 'jj git push -b x'

# Missing revisions, remotes, and unsupported arguments cannot silently select zero commits.
for invocation in \
  'jj git push -b does-not-exist' \
  'jj git push --remote does-not-exist -b main' \
  'jj git push -r does-not-exist' \
  'jj git push --unknown-option' \
  'jj -R does-not-exist git push -b main' \
  'jj git push -b main --config git.push=empty' \
  'jj git push -b main; true'; do
  expect_claude_block "$invocation"
done
push_cwd="$push_fixture"
expect_claude_block 'jj git push -b main'
push_cwd="$push_fixture/sender"

# Match gitleaks/pre-commit: binary files are skipped rather than blocking the push.
fixture_jj new x -m 'Binary fixture workspace'
printf '\000binary fixture\n' >"$push_fixture/sender/binary.bin"
fixture_jj commit binary.bin -m 'Binary scanner boundary'
fixture_jj bookmark create binary -r @-
expect_claude_allow 'jj git push -b binary'

# Non-UTF-8 text still reaches the scanner, both without and with a secret.
fixture_jj new x -m 'Non-UTF-8 fixture workspace'
printf 'caf\351\n' >"$push_fixture/sender/latin.txt"
fixture_jj commit latin.txt -m 'Latin-1 clean text'
fixture_jj bookmark create latin -r @-
expect_claude_allow 'jj git push -b latin'
printf 'fixture_value=%s\n' "$synthetic_pat" >>"$push_fixture/sender/latin.txt"
fixture_jj commit latin.txt -m 'Latin-1 synthetic positive'
fixture_jj bookmark create latin-secret -r @-
latin_commit=$("$real_jj" --repository "$push_fixture/sender" log --ignore-working-copy -r latin-secret --no-graph -T commit_id)
expect_secret_block 'jj git push -b latin-secret' "$latin_commit" latin.txt 2

# Diff lines are LF-delimited: these characters remain inside one source line.
separator_case=0
for separator in $'\r' $'\f' $'\342\200\250'; do
  separator_case=$((separator_case + 1))
  fixture_jj new x -m 'Inline separator fixture'
  printf 'left%sright\n' "$separator" >"$push_fixture/sender/separator.txt"
  fixture_jj commit separator.txt -m 'Clean inline separator'
  fixture_jj bookmark create "separator-clean-$separator_case" -r @-
  expect_claude_allow "jj git push -b separator-clean-$separator_case"
  printf 'left%sfixture_value=%s\n' "$separator" "$synthetic_pat" >"$push_fixture/sender/separator.txt"
  fixture_jj commit separator.txt -m 'Synthetic secret after inline separator'
  fixture_jj bookmark create "separator-secret-$separator_case" -r @-
  separator_commit=$("$real_jj" --repository "$push_fixture/sender" log --ignore-working-copy \
    -r "separator-secret-$separator_case" --no-graph -T commit_id)
  expect_secret_block "jj git push -b separator-secret-$separator_case" "$separator_commit" separator.txt 1
done

# Build an already-published synthetic baseline directly in an independent local receiver.
# No secret is pushed: a local clone reads it, then only hook calls inspect removals.
"$real_jj" git init --no-colocate "$push_fixture/published-receiver" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
printf 'keep this line\nfixture_value=%s\n' "$synthetic_pat" >"$push_fixture/published-receiver/published.txt"
(
  cd "$push_fixture/published-receiver"
  "$real_jj" commit published.txt -m 'Synthetic published baseline'
  "$real_jj" bookmark create main -r @-
  "$real_jj" git export
) >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
"$real_jj" git clone --no-colocate "$push_fixture/published-receiver/.jj/repo/store/git" \
  "$push_fixture/published-sender" >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
published_jj() {
  (cd "$push_fixture/published-sender" && "$real_jj" "$@") >"$push_fixture/setup.stdout" 2>"$push_fixture/setup.stderr"
}
published_jj new main@origin -m 'Remove published synthetic line'
printf 'keep this line\n' >"$push_fixture/published-sender/published.txt"
published_jj commit published.txt -m 'Remove published synthetic line'
published_jj bookmark set main -r @-
push_cwd="$push_fixture/published-sender"
expect_claude_allow 'jj git push -b main'
published_jj new main@origin -m 'Remove published synthetic file'
rm "$push_fixture/published-sender/published.txt"
published_jj commit published.txt -m 'Remove published synthetic file'
published_jj bookmark create remove-file -r @-
expect_claude_allow 'jj git push -b remove-file'
push_cwd="$push_fixture/sender"

# The local receiver still contains only the clean baseline; hook calls never push.
checked=$((checked + 1))
receiver_tip=$(git --git-dir="$push_fixture/receiver/.jj/repo/store/git" rev-parse refs/heads/main)
remote_tip=$("$real_jj" --repository "$push_fixture/sender" log --ignore-working-copy -r main@origin --no-graph -T commit_id)
[[ $receiver_tip == "$remote_tip" ]] || fail 'hook changed the local receiver'
if git --git-dir="$push_fixture/receiver/.jj/repo/store/git" cat-file -e "$secret_commit" 2>/dev/null; then
  fail 'synthetic secret commit reached the local receiver'
fi
cleanup
trap - EXIT
checked=$((checked + 1))
[[ ! -e $push_fixture && ! -e $stderr_file ]] || fail 'fixture cleanup left temporary files'
echo "block-destructive-git: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
