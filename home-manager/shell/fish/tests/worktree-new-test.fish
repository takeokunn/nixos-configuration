#!/usr/bin/env fish
# Behavioral test for worktree_new / __fzf_ghq_new_worktree.
#
# No fish test framework (bats, fishtape) is vendored in this repo, so this
# exercises the functions directly by sourcing them into a subshell and
# driving them against scratch git repos created fresh under `mktemp -d`,
# the same technique used for manual verification of this change. Every
# scratch path is created here and removed at the end; nothing outside a
# fresh mktemp -d is read or written.
#
# Usage: fish home-manager/shell/fish/tests/worktree-new-test.fish
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

# --- 1. syntax check on all three touched files -----------------------------

for f in worktree_new.fish __fzf_ghq_new_worktree.fish fzf_ghq.fish
    if fish -n "$func_dir/$f" 2>/tmp/worktree-new-test-syntax-err.$fish_pid
        _pass "fish -n $f"
    else
        _fail "fish -n $f: "(cat /tmp/worktree-new-test-syntax-err.$fish_pid)
    end
    rm -f /tmp/worktree-new-test-syntax-err.$fish_pid
end

# --- 2. success path: worktree_new creates a worktree and cds into it -------

set -l ok_root (mktemp -d)
mkdir -p $ok_root/src
git init -q $ok_root/src
git -C $ok_root/src commit -q --allow-empty -m init
git clone -q --bare $ok_root/src $ok_root/repo.git

set -l expected_short_sha (git -C $ok_root/repo.git rev-parse --short HEAD)
set -l expected_full_sha (git -C $ok_root/repo.git rev-parse HEAD)
set -l repo_git (path resolve $ok_root/repo.git)

set -l ok_lines (fish -c "
    source $func_dir/__fzf_ghq_new_worktree.fish
    source $func_dir/worktree_new.fish
    cd $repo_git
    worktree_new
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
")

set -l ok_exit (string replace 'EXIT_STATUS=' '' -- $ok_lines[1])
set -l ok_pwd (string replace 'PWD_AFTER=' '' -- $ok_lines[2])

if test "$ok_exit" = 0
    _pass "worktree_new exits 0 on success"
else
    _fail "worktree_new exit status: expected 0, got '$ok_exit'"
end

set -l ok_pwd_pattern "^$repo_git/\.worktrees/[0-9]{8}T[0-9]{6}-$expected_short_sha\$"
if string match -qr -- $ok_pwd_pattern $ok_pwd
    _pass "\$PWD after worktree_new matches <repo>/.worktrees/<timestamp>-<shortsha>"
else
    _fail "\$PWD after worktree_new: expected to match $ok_pwd_pattern, got '$ok_pwd'"
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

if test -n "$ok_pwd"; and git -C $repo_git worktree list --porcelain | string match -q "worktree $ok_pwd"
    _pass "worktree is registered in 'git worktree list'"
else
    _fail "worktree '$ok_pwd' is not registered in 'git worktree list' for $repo_git"
end

rm -rf $ok_root

# --- 3. failure path: no git repo above cwd -> exit 1, $PWD unchanged ------

set -l fail_root (mktemp -d)
set -l fail_dir (path resolve $fail_root)

if git -C $fail_dir rev-parse --show-toplevel >/dev/null 2>&1
    _fail "precondition violated: $fail_dir is unexpectedly inside a git repository, cannot exercise the failure path"
else
    set -l fail_lines (fish -c "
        source $func_dir/__fzf_ghq_new_worktree.fish
        source $func_dir/worktree_new.fish
        cd $fail_dir
        worktree_new
        echo EXIT_STATUS=\$status
        echo PWD_AFTER=\$PWD
    ")

    set -l fail_exit (string replace 'EXIT_STATUS=' '' -- $fail_lines[1])
    set -l fail_pwd (string replace 'PWD_AFTER=' '' -- $fail_lines[2])

    if test "$fail_exit" = 1
        _pass "worktree_new exits 1 with no git repository above \$PWD"
    else
        _fail "worktree_new exit status outside a git repo: expected 1, got '$fail_exit'"
    end

    if test "$fail_pwd" = "$fail_dir"
        _pass "\$PWD is unchanged after a failed worktree_new"
    else
        _fail "\$PWD changed after a failed worktree_new: expected '$fail_dir', got '$fail_pwd'"
    end
end

rm -rf $fail_root

# --- summary -----------------------------------------------------------------

if test $failures -eq 0
    echo "worktree-new-test: all checks passed"
    exit 0
else
    echo "worktree-new-test: $failures check(s) failed"
    exit 1
end
