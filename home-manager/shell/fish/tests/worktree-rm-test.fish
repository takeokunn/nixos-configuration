#!/usr/bin/env fish
# Behavioral test for worktree_rm.
#
# No fish test framework (bats, fishtape) is vendored in this repo, so this
# exercises the function directly by sourcing it into a subshell and driving
# it against scratch git repos created fresh under `mktemp -d`, the same
# technique used by the sibling worktree-new-test.fish and for manual
# verification of this change. Every scratch path is created here and
# removed at the end; nothing outside a fresh mktemp -d is read or written.
#
# Usage: fish home-manager/shell/fish/tests/worktree-rm-test.fish
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

# --- 1. syntax check on the touched file -------------------------------------

for f in worktree_rm.fish
    if fish -n "$func_dir/$f" 2>/tmp/worktree-rm-test-syntax-err.$fish_pid
        _pass "fish -n $f"
    else
        _fail "fish -n $f: "(cat /tmp/worktree-rm-test-syntax-err.$fish_pid)
    end
    rm -f /tmp/worktree-rm-test-syntax-err.$fish_pid
end

# --- 2. success path: worktree_rm removes a clean worktree and cds to the ---
# ---    bare repo root -------------------------------------------------------

set -l ok_root (mktemp -d)
mkdir -p $ok_root/src
git init -q $ok_root/src
git -C $ok_root/src commit -q --allow-empty -m init
git clone -q --bare $ok_root/src $ok_root/repo.git

set -l repo_git (path resolve $ok_root/repo.git)
set -l clean_wt $repo_git/.worktrees/wt-clean

git -C $repo_git worktree add -q --detach $clean_wt

if test -d "$clean_wt"
    set -l clean_lines (fish -c "
        source $func_dir/worktree_rm.fish
        cd $clean_wt
        worktree_rm 2>/tmp/worktree-rm-test-clean-err.$fish_pid
        echo EXIT_STATUS=\$status
        echo PWD_AFTER=\$PWD
    ")
    rm -f /tmp/worktree-rm-test-clean-err.$fish_pid

    set -l clean_exit (string replace 'EXIT_STATUS=' '' -- $clean_lines[1])
    set -l clean_pwd (string replace 'PWD_AFTER=' '' -- $clean_lines[2])

    if test "$clean_exit" = 0
        _pass "worktree_rm exits 0 removing a clean worktree"
    else
        _fail "worktree_rm exit status on clean removal: expected 0, got '$clean_exit'"
    end

    if test -n "$clean_pwd"; and test (path resolve "$clean_pwd") = "$repo_git"
        _pass "\$PWD after worktree_rm equals the bare repo root"
    else
        _fail "\$PWD after worktree_rm: expected '$repo_git', got '$clean_pwd'"
    end

    if test -d "$clean_wt"
        _fail "removed worktree directory '$clean_wt' still exists on disk"
    else
        _pass "removed worktree directory no longer exists on disk"
    end
else
    _fail "precondition violated: could not create linked worktree at $clean_wt"
end

# --- 3. bare-root refusal: worktree_rm at the bare repo root itself ---------

set -l bare_lines (fish -c "
    source $func_dir/worktree_rm.fish
    cd $repo_git
    worktree_rm 2>/tmp/worktree-rm-test-bare-err.$fish_pid
    echo EXIT_STATUS=\$status
")
set -l bare_exit (string replace 'EXIT_STATUS=' '' -- $bare_lines[1])
set -l bare_stderr (cat /tmp/worktree-rm-test-bare-err.$fish_pid)
rm -f /tmp/worktree-rm-test-bare-err.$fish_pid

if test "$bare_exit" = 1
    _pass "worktree_rm exits 1 at the bare repo root"
else
    _fail "worktree_rm exit status at the bare repo root: expected 1, got '$bare_exit'"
end

if string match -q '*worktree_rm: not inside a git worktree*' -- $bare_stderr
    _pass "worktree_rm reports 'not inside a git worktree' at the bare repo root"
else
    _fail "worktree_rm stderr at the bare repo root: expected to contain 'worktree_rm: not inside a git worktree', got '$bare_stderr'"
end

rm -rf $ok_root

# --- 4. dirty refusal: worktree_rm without --force on an untracked file ----

set -l dirty_root (mktemp -d)
mkdir -p $dirty_root/src
git init -q $dirty_root/src
git -C $dirty_root/src commit -q --allow-empty -m init
git clone -q --bare $dirty_root/src $dirty_root/repo.git

set -l dirty_repo_git (path resolve $dirty_root/repo.git)
set -l dirty_wt $dirty_repo_git/.worktrees/wt-dirty

git -C $dirty_repo_git worktree add -q --detach $dirty_wt

if test -d "$dirty_wt"
    echo untracked >$dirty_wt/untracked.txt

    set -l dirty_lines (fish -c "
        source $func_dir/worktree_rm.fish
        cd $dirty_wt
        worktree_rm 2>/tmp/worktree-rm-test-dirty-err.$fish_pid
        echo EXIT_STATUS=\$status
        echo PWD_AFTER=\$PWD
    ")
    set -l dirty_exit (string replace 'EXIT_STATUS=' '' -- $dirty_lines[1])
    set -l dirty_pwd (string replace 'PWD_AFTER=' '' -- $dirty_lines[2])
    set -l dirty_stderr (cat /tmp/worktree-rm-test-dirty-err.$fish_pid)
    rm -f /tmp/worktree-rm-test-dirty-err.$fish_pid

    if test "$dirty_exit" = 1
        _pass "worktree_rm exits 1 on a dirty worktree without --force"
    else
        _fail "worktree_rm exit status on a dirty worktree without --force: expected 1, got '$dirty_exit'"
    end

    if string match -q '*contains modified or untracked files, use --force to delete it*' -- $dirty_stderr
        _pass "worktree_rm surfaces git's own dirty-worktree refusal text"
    else
        _fail "worktree_rm stderr on a dirty worktree: expected to contain git's 'use --force to delete it' refusal, got '$dirty_stderr'"
    end

    if test -n "$dirty_pwd"; and test (path resolve "$dirty_pwd") = "$dirty_wt"
        _pass "\$PWD is unchanged after a refused dirty removal"
    else
        _fail "\$PWD after a refused dirty removal: expected '$dirty_wt', got '$dirty_pwd'"
    end

    if test -d "$dirty_wt"
        _pass "dirty worktree directory still exists on disk after refusal"
    else
        _fail "dirty worktree directory '$dirty_wt' no longer exists after a refused removal"
    end

    # --- 5. forced removal: worktree_rm --force removes the same dirty ---------
    # ---    worktree and cds to the bare repo root ------------------------------

    set -l force_lines (fish -c "
        source $func_dir/worktree_rm.fish
        cd $dirty_wt
        worktree_rm --force 2>/tmp/worktree-rm-test-force-err.$fish_pid
        echo EXIT_STATUS=\$status
        echo PWD_AFTER=\$PWD
    ")
    set -l force_exit (string replace 'EXIT_STATUS=' '' -- $force_lines[1])
    set -l force_pwd (string replace 'PWD_AFTER=' '' -- $force_lines[2])
    rm -f /tmp/worktree-rm-test-force-err.$fish_pid

    if test "$force_exit" = 0
        _pass "worktree_rm --force exits 0 on a dirty worktree"
    else
        _fail "worktree_rm --force exit status on a dirty worktree: expected 0, got '$force_exit'"
    end

    if test -n "$force_pwd"; and test (path resolve "$force_pwd") = "$dirty_repo_git"
        _pass "\$PWD after worktree_rm --force equals the bare repo root"
    else
        _fail "\$PWD after worktree_rm --force: expected '$dirty_repo_git', got '$force_pwd'"
    end

    if test -d "$dirty_wt"
        _fail "forcibly removed worktree directory '$dirty_wt' still exists on disk"
    else
        _pass "forcibly removed worktree directory no longer exists on disk"
    end
else
    _fail "precondition violated: could not create linked worktree at $dirty_wt"
end

rm -rf $dirty_root

# --- summary -----------------------------------------------------------------

if test $failures -eq 0
    echo "worktree-rm-test: all checks passed"
    exit 0
else
    echo "worktree-rm-test: $failures check(s) failed"
    exit 1
end
