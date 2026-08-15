#!/usr/bin/env fish
# Behavioral test for worktree_default_branch / __fzf_ghq_new_worktree's
# "branch" naming mode (3rd positional arg).
#
# No fish test framework (bats, fishtape) is vendored in this repo, so this
# exercises the functions directly by sourcing them into a subshell and
# driving them against scratch git repos created fresh under `mktemp -d`,
# the same technique used by worktree-new-test.fish. Every scratch path is
# created here and removed at the end; nothing outside a fresh mktemp -d is
# read or written.
#
# Usage: fish home-manager/shell/fish/tests/worktree-default-branch-test.fish
# Exit status: 0 if every assertion passed, 1 otherwise.

set -l func_dir (path resolve (path dirname (status --current-filename))/../functions)

set -g failures 0

function _pass
    echo "PASS: $argv[1]"
end

function _fail
    set -g failures (math $failures + 1)
    echo "FAIL: $argv[1]"
end

# --- 1. syntax check on both touched files -----------------------------

for f in __fzf_ghq_new_worktree.fish worktree_default_branch.fish
    if fish -n "$func_dir/$f" 2>/tmp/worktree-default-branch-test-syntax-err.$fish_pid
        _pass "fish -n $f"
    else
        _fail "fish -n $f: "(cat /tmp/worktree-default-branch-test-syntax-err.$fish_pid)
    end
    rm -f /tmp/worktree-default-branch-test-syntax-err.$fish_pid
end

# --- 2. success path: names the worktree after the default branch, not a
#        short sha, and cds into it -------------------------------------

set -l ok_root (mktemp -d)
mkdir -p $ok_root/src
git init -q $ok_root/src
git -C $ok_root/src commit -q --allow-empty -m init
git clone -q --bare $ok_root/src $ok_root/repo.git

set -l repo_git (path resolve $ok_root/repo.git)
set -l expected_branch (git -C $repo_git symbolic-ref --short HEAD)
set -l expected_full_sha (git -C $repo_git rev-parse HEAD)
set -l branches_before (git -C $repo_git branch --list)

set -l ok_lines (fish -c "
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/__fzf_ghq_new_worktree.fish
    source $func_dir/worktree_default_branch.fish
    cd $repo_git
    worktree_default_branch
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
")

set -l ok_exit (string replace 'EXIT_STATUS=' '' -- $ok_lines[1])
set -l ok_pwd (string replace 'PWD_AFTER=' '' -- $ok_lines[2])

if test "$ok_exit" = 0
    _pass "worktree_default_branch exits 0 on success"
else
    _fail "worktree_default_branch exit status: expected 0, got '$ok_exit'"
end

set -l ok_pwd_pattern "^$repo_git/\.worktrees/[0-9]{8}T[0-9]{6}-$expected_branch\$"
if string match -qr -- $ok_pwd_pattern $ok_pwd
    _pass "\$PWD after worktree_default_branch matches <repo>/.worktrees/<timestamp>-<default-branch> (not a short-sha)"
else
    _fail "\$PWD after worktree_default_branch: expected to match $ok_pwd_pattern, got '$ok_pwd'"
end

if test -d "$ok_pwd"
    _pass "created worktree directory exists on disk"
else
    _fail "created worktree directory '$ok_pwd' does not exist"
end

if test -n "$ok_pwd"; and test (git -C "$ok_pwd" rev-parse HEAD 2>/dev/null) = "$expected_full_sha"
    _pass "created worktree HEAD matches the bare repo's commit"
else
    _fail "created worktree HEAD does not match expected commit $expected_full_sha"
end

# FR-002 regression lock: no creation path -- including this branch-naming
# one -- ever creates a local branch. `git branch --list` inside the new
# worktree is NOT expected to be empty in absolute terms: the shared bare
# repo already has its one branch (e.g. "main") before this call, and that
# branch stays listed (marked with a `+`, since the bare repo's own HEAD
# symref points at it) regardless of what this function does. The
# regression-proof assertion is therefore that the branch *set* is
# unchanged by the call, combined with the worktree itself carrying a
# detached HEAD.
set -l branches_after (git -C $repo_git branch --list)
if test "$branches_before" = "$branches_after"
    _pass "worktree_default_branch creates no new local branch (branch --list unchanged)"
else
    _fail "branch --list changed: before='$branches_before' after='$branches_after'"
end

if test -n "$ok_pwd"; and not git -C "$ok_pwd" symbolic-ref -q HEAD >/dev/null 2>&1
    _pass "created worktree HEAD is detached (symbolic-ref -q HEAD fails)"
else
    _fail "created worktree HEAD is not detached (symbolic-ref -q HEAD unexpectedly succeeded)"
end

rm -rf $ok_root

# --- 3. a default branch name containing '/' is flattened, not nested ---

set -l slash_root (mktemp -d)
mkdir -p $slash_root/src
git init -q $slash_root/src
git -C $slash_root/src symbolic-ref HEAD refs/heads/release/1.0
git -C $slash_root/src commit -q --allow-empty -m init
git clone -q --bare $slash_root/src $slash_root/repo.git

set -l slash_repo_git (path resolve $slash_root/repo.git)
set -l slash_expected_branch (git -C $slash_repo_git symbolic-ref --short HEAD)

if test "$slash_expected_branch" = "release/1.0"
    _pass "precondition: scratch repo's default branch is 'release/1.0'"
else
    _fail "precondition violated: scratch repo's default branch is '$slash_expected_branch', expected 'release/1.0'"
end

set -l slash_lines (fish -c "
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/__fzf_ghq_new_worktree.fish
    source $func_dir/worktree_default_branch.fish
    cd $slash_repo_git
    worktree_default_branch
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
")

set -l slash_exit (string replace 'EXIT_STATUS=' '' -- $slash_lines[1])
set -l slash_pwd (string replace 'PWD_AFTER=' '' -- $slash_lines[2])

if test "$slash_exit" = 0
    _pass "worktree_default_branch exits 0 for a default branch containing '/'"
else
    _fail "worktree_default_branch exit status for slash branch: expected 0, got '$slash_exit'"
end

set -l slash_pwd_pattern "^$slash_repo_git/\.worktrees/[0-9]{8}T[0-9]{6}-release-1\.0\$"
if string match -qr -- $slash_pwd_pattern $slash_pwd
    _pass "'/' in the default branch name is replaced with '-' in the directory name, not nested"
else
    _fail "\$PWD for slash branch: expected to match $slash_pwd_pattern, got '$slash_pwd'"
end

if test -d "$slash_pwd"
    _pass "created worktree directory for slash branch exists on disk (flattened, not a nested path)"
else
    _fail "created worktree directory '$slash_pwd' does not exist"
end

rm -rf $slash_root

# --- 4. collision-avoidance loop is exercised without erroring ----------

set -l coll_root (mktemp -d)
mkdir -p $coll_root/src
git init -q $coll_root/src
git -C $coll_root/src commit -q --allow-empty -m init
git clone -q --bare $coll_root/src $coll_root/repo.git

set -l coll_repo_git (path resolve $coll_root/repo.git)

set -l coll_lines (fish -c "
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/__fzf_ghq_new_worktree.fish
    source $func_dir/worktree_default_branch.fish
    cd $coll_repo_git
    worktree_default_branch
    set -l first_exit \$status
    set -l first_pwd \$PWD
    cd $coll_repo_git
    worktree_default_branch
    set -l second_exit \$status
    set -l second_pwd \$PWD
    echo FIRST_EXIT=\$first_exit
    echo FIRST_PWD=\$first_pwd
    echo SECOND_EXIT=\$second_exit
    echo SECOND_PWD=\$second_pwd
")

set -l coll_first_exit (string replace 'FIRST_EXIT=' '' -- $coll_lines[1])
set -l coll_first_pwd (string replace 'FIRST_PWD=' '' -- $coll_lines[2])
set -l coll_second_exit (string replace 'SECOND_EXIT=' '' -- $coll_lines[3])
set -l coll_second_pwd (string replace 'SECOND_PWD=' '' -- $coll_lines[4])

if test "$coll_first_exit" = 0
    _pass "collision loop: first worktree_default_branch call exits 0"
else
    _fail "collision loop: first call exit status: expected 0, got '$coll_first_exit'"
end

if test "$coll_second_exit" = 0
    _pass "collision loop: second worktree_default_branch call exits 0"
else
    _fail "collision loop: second call exit status: expected 0, got '$coll_second_exit'"
end

if test -d "$coll_first_pwd"
    _pass "collision loop: first created directory exists on disk"
else
    _fail "collision loop: first created directory '$coll_first_pwd' does not exist"
end

if test -d "$coll_second_pwd"
    _pass "collision loop: second created directory exists on disk"
else
    _fail "collision loop: second created directory '$coll_second_pwd' does not exist"
end

if test -n "$coll_first_pwd"; and test -n "$coll_second_pwd"; and test "$coll_first_pwd" != "$coll_second_pwd"
    _pass "collision loop: two calls in the same repo produce two distinct worktree paths"
else
    _fail "collision loop: expected two distinct paths, got '$coll_first_pwd' and '$coll_second_pwd'"
end

rm -rf $coll_root

# --- summary -----------------------------------------------------------------

if test $failures -eq 0
    echo "worktree-default-branch-test: all checks passed"
    exit 0
else
    echo "worktree-default-branch-test: $failures check(s) failed"
    exit 1
end
