#!/usr/bin/env fish
# Behavioral test for worktree_switch.
#
# Mirrors worktree-new-test.fish's technique: source the function into a
# fresh `fish -c "..."` subshell (never into the current process) and drive
# it against scratch git repos created fresh under `mktemp -d`. Every
# scratch path is created here and removed at the end; nothing outside a
# fresh mktemp -d is read or written.
#
# The interactive `fzf` picker is stubbed via a fake `fzf` executable placed
# ahead of $PATH -- the same technique fzf_ghq.test.fish uses to stub
# yq-go -- so the selection and cancel paths can be driven deterministically
# without a real terminal.
#
# Usage: fish home-manager/shell/fish/tests/worktree-switch-test.fish
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

# Writes a stub `fzf` into $stub_dir that reads its stdin and, when
# $FZF_STUB_SELECT is set in the invoking environment, echoes the first
# line back (simulating a selection); otherwise it echoes nothing
# (simulating the user cancelling the picker with Esc/Ctrl-C).
function make_fzf_stub -a stub_dir
    begin
        echo '#!/usr/bin/env fish'
        echo 'set -l lines (cat)'
        echo 'if test -n "$FZF_STUB_SELECT"'
        echo '    printf "%s\n" $lines[1]'
        echo 'end'
    end >"$stub_dir/fzf"
    chmod +x "$stub_dir/fzf"
end

# --- 1. syntax check on the touched file -------------------------------------

if fish -n "$func_dir/worktree_switch.fish" 2>/tmp/worktree-switch-test-syntax-err.$fish_pid
    _pass "fish -n worktree_switch.fish"
else
    _fail "fish -n worktree_switch.fish: "(cat /tmp/worktree-switch-test-syntax-err.$fish_pid)
end
rm -f /tmp/worktree-switch-test-syntax-err.$fish_pid

# --- 2. zero-worktree case: exit 1, $PWD unchanged, stderr message ----------

set -l zero_root (mktemp -d)
mkdir -p $zero_root/src
git init -q $zero_root/src
git -C $zero_root/src commit -q --allow-empty -m init
git clone -q --bare $zero_root/src $zero_root/repo.git
set -l zero_repo_git (path resolve $zero_root/repo.git)

set -l zero_lines (fish -c "
    source $func_dir/worktree_switch.fish
    cd $zero_repo_git
    worktree_switch
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
" 2>/tmp/worktree-switch-test-zero-stderr.$fish_pid)

set -l zero_exit (string replace 'EXIT_STATUS=' '' -- $zero_lines[1])
set -l zero_pwd (string replace 'PWD_AFTER=' '' -- $zero_lines[2])
set -l zero_stderr (cat /tmp/worktree-switch-test-zero-stderr.$fish_pid)
rm -f /tmp/worktree-switch-test-zero-stderr.$fish_pid

if test "$zero_exit" = 1
    _pass "worktree_switch exits 1 with zero other worktrees"
else
    _fail "worktree_switch exit status with zero other worktrees: expected 1, got '$zero_exit'"
end

if test "$zero_pwd" = "$zero_repo_git"
    _pass "\$PWD unchanged after zero-worktree failure"
else
    _fail "\$PWD after zero-worktree failure: expected '$zero_repo_git', got '$zero_pwd'"
end

if string match -q '*no worktrees found*' -- $zero_stderr
    _pass "zero-worktree case prints a stderr message"
else
    _fail "zero-worktree case stderr message: expected to mention 'no worktrees found', got '$zero_stderr'"
end

rm -rf $zero_root

# --- 3. successful selection: stubbed fzf picks the one candidate ----------

set -l sel_root (mktemp -d)
mkdir -p $sel_root/src
git init -q $sel_root/src
git -C $sel_root/src commit -q --allow-empty -m init
git clone -q --bare $sel_root/src $sel_root/repo.git
set -l sel_repo_git (path resolve $sel_root/repo.git)
git -C $sel_repo_git worktree add -q --detach $sel_root/wt1 HEAD >/dev/null 2>&1
set -l sel_wt1 (path resolve $sel_root/wt1)

set -l sel_stub_dir (mktemp -d)
make_fzf_stub $sel_stub_dir

set -l sel_lines (fish -c "
    set -lx PATH $sel_stub_dir \$PATH
    set -lx FZF_STUB_SELECT 1
    source $func_dir/worktree_switch.fish
    cd $sel_repo_git
    worktree_switch
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
")

set -l sel_exit (string replace 'EXIT_STATUS=' '' -- $sel_lines[1])
set -l sel_pwd (string replace 'PWD_AFTER=' '' -- $sel_lines[2])

if test "$sel_exit" = 0
    _pass "worktree_switch exits 0 on a successful fzf selection"
else
    _fail "worktree_switch exit status on selection: expected 0, got '$sel_exit'"
end

if test "$sel_pwd" = "$sel_wt1"
    _pass "\$PWD after selection is the picked worktree"
else
    _fail "\$PWD after selection: expected '$sel_wt1', got '$sel_pwd'"
end

rm -rf $sel_root $sel_stub_dir

# --- 4. cancel path: stubbed fzf echoes nothing -> no cd, exit 0 -----------

set -l cancel_root (mktemp -d)
mkdir -p $cancel_root/src
git init -q $cancel_root/src
git -C $cancel_root/src commit -q --allow-empty -m init
git clone -q --bare $cancel_root/src $cancel_root/repo.git
set -l cancel_repo_git (path resolve $cancel_root/repo.git)
git -C $cancel_repo_git worktree add -q --detach $cancel_root/wt1 HEAD >/dev/null 2>&1

set -l cancel_stub_dir (mktemp -d)
make_fzf_stub $cancel_stub_dir

set -l cancel_lines (fish -c "
    set -lx PATH $cancel_stub_dir \$PATH
    source $func_dir/worktree_switch.fish
    cd $cancel_repo_git
    worktree_switch
    echo EXIT_STATUS=\$status
    echo PWD_AFTER=\$PWD
")

set -l cancel_exit (string replace 'EXIT_STATUS=' '' -- $cancel_lines[1])
set -l cancel_pwd (string replace 'PWD_AFTER=' '' -- $cancel_lines[2])

if test "$cancel_exit" = 0
    _pass "worktree_switch exits 0 when the fzf picker is cancelled"
else
    _fail "worktree_switch exit status on cancel: expected 0, got '$cancel_exit'"
end

if test "$cancel_pwd" = "$cancel_repo_git"
    _pass "\$PWD unchanged after cancelling the picker (no cd happened)"
else
    _fail "\$PWD after cancel: expected '$cancel_repo_git', got '$cancel_pwd'"
end

rm -rf $cancel_root $cancel_stub_dir

# --- 5. failure path: not inside a git repo -> exit 1, $PWD unchanged ------

set -l notrepo_root (mktemp -d)
set -l notrepo_dir (path resolve $notrepo_root)

if git -C $notrepo_dir rev-parse --show-toplevel >/dev/null 2>&1
    _fail "precondition violated: $notrepo_dir is unexpectedly inside a git repository, cannot exercise the failure path"
else
    set -l notrepo_lines (fish -c "
        source $func_dir/worktree_switch.fish
        cd $notrepo_dir
        worktree_switch
        echo EXIT_STATUS=\$status
        echo PWD_AFTER=\$PWD
    " 2>/tmp/worktree-switch-test-notrepo-stderr.$fish_pid)

    set -l notrepo_exit (string replace 'EXIT_STATUS=' '' -- $notrepo_lines[1])
    set -l notrepo_pwd (string replace 'PWD_AFTER=' '' -- $notrepo_lines[2])
    set -l notrepo_stderr (cat /tmp/worktree-switch-test-notrepo-stderr.$fish_pid)
    rm -f /tmp/worktree-switch-test-notrepo-stderr.$fish_pid

    if test "$notrepo_exit" = 1
        _pass "worktree_switch exits 1 outside a git repository"
    else
        _fail "worktree_switch exit status outside a git repo: expected 1, got '$notrepo_exit'"
    end

    if test "$notrepo_pwd" = "$notrepo_dir"
        _pass "\$PWD unchanged after failing outside a git repository"
    else
        _fail "\$PWD outside a git repo: expected '$notrepo_dir', got '$notrepo_pwd'"
    end

    if string match -q '*not inside a git repository*' -- $notrepo_stderr
        _pass "not-a-git-repo case prints a stderr message"
    else
        _fail "not-a-git-repo case stderr message: expected to mention 'not inside a git repository', got '$notrepo_stderr'"
    end
end

rm -rf $notrepo_root

# --- summary -----------------------------------------------------------------

if test $failures -eq 0
    echo "worktree-switch-test: all checks passed"
    exit 0
else
    echo "worktree-switch-test: $failures check(s) failed"
    exit 1
end
