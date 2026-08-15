#!/usr/bin/env fish
# Behavioral test for worktree_clean.
#
# Mirrors worktree-switch-test.fish's technique: source the function into a
# fresh `fish -c "..."` subshell (never into the current process) and drive
# it against scratch git repos created fresh under `mktemp -d`. Every
# scratch path is created here and removed at the end; nothing outside a
# fresh mktemp -d is read or written.
#
# The interactive `fzf --multi` picker is stubbed via a fake `fzf`
# executable placed ahead of $PATH, extending worktree-switch-test.fish's
# single-select stub to echo back an arbitrary set of stdin lines chosen by
# 1-based index, so a multi-select can be driven deterministically without a
# real terminal.
#
# `ghq list --full-path` (worktree_clean's enumeration source) is pointed at
# a scratch GHQ_ROOT for every scenario, built the same way for each: a bare
# origin, a seed clone that creates the initial commit, a push to the
# origin's main, then a bare clone of that origin into the scratch GHQ_ROOT
# tree -- the same layout `ghq get` produces for a real repository.
#
# Usage: fish home-manager/shell/fish/tests/worktree-clean-test.fish
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
# $FZF_STUB_SELECT_INDICES is set in the invoking environment to a
# space-separated list of 1-based line numbers, echoes back exactly those
# lines (simulating a multi-select); otherwise it echoes nothing (simulating
# the user cancelling the picker with Esc/Ctrl-C).
#
# Also copies everything it read on stdin -- the full set of candidate
# lines worktree_clean built, signal labels included -- to
# $stub_dir/fzf-stdin.log, so a test can inspect exactly what the picker was
# offered rather than only what came back selected.
function make_fzf_stub -a stub_dir
    begin
        echo '#!/usr/bin/env fish'
        echo 'set -l lines (cat)'
        echo 'printf "%s\n" $lines > '"$stub_dir/fzf-stdin.log"
        echo 'if test -n "$FZF_STUB_SELECT_INDICES"'
        echo '    for idx in (string split " " -- $FZF_STUB_SELECT_INDICES)'
        echo '        printf "%s\n" $lines[$idx]'
        echo '    end'
        echo end
    end >"$stub_dir/fzf"
    chmod +x "$stub_dir/fzf"
end

# Commits an empty commit in $repo_dir with author/committer dates backdated
# to $epoch (a Unix timestamp). `GIT_AUTHOR_DATE`/`GIT_COMMITTER_DATE` do not
# accept git's free-form approxidate syntax ("4 days ago" fails with "fatal:
# invalid date format") -- only "@<epoch> <tz>" or a small set of fixed
# formats -- so the caller computes the epoch itself.
function commit_backdated -a repo_dir epoch message
    set -lx GIT_AUTHOR_DATE "@$epoch +0000"
    set -lx GIT_COMMITTER_DATE "@$epoch +0000"
    git -c user.email=t@t -c user.name=t -C $repo_dir commit -q --allow-empty -m "$message"
end

# Builds a bare repo at $repo_git_dir (parent dirs created as needed) with a
# single initial commit on main, matching a ghq-managed bare clone's layout.
# When $epoch is non-empty, that initial commit's date is backdated to it --
# used to construct a worktree that is merged AND idle without amending its
# HEAD (which would create a commit no longer identical to origin/main).
#
# worktree_clean fetches from each repo's "origin" remote to resolve the
# default branch, so the throwaway origin/seed scratch dir this function
# creates must stay alive for the test's duration; its path is returned so
# the caller can remove it during teardown.
function make_bare_repo -a repo_git_dir epoch
    mkdir -p (path dirname $repo_git_dir)
    set -l scratch (mktemp -d)
    mkdir -p $scratch/origin
    git init -q --bare $scratch/origin
    git -C $scratch/origin symbolic-ref HEAD refs/heads/main
    git clone -q $scratch/origin $scratch/seed
    if test -n "$epoch"
        commit_backdated $scratch/seed $epoch init
    else
        git -c user.email=t@t -c user.name=t -C $scratch/seed commit -q --allow-empty -m init
    end
    git -C $scratch/seed push -q origin HEAD:refs/heads/main
    git clone -q --bare $scratch/origin $repo_git_dir
    git -C $repo_git_dir symbolic-ref HEAD refs/heads/main
    echo $scratch
end

# --- 1. syntax check on the touched file -------------------------------------

if fish -n "$func_dir/worktree_clean.fish" 2>/tmp/worktree-clean-test-syntax-err.$fish_pid
    _pass "fish -n worktree_clean.fish"
else
    _fail "fish -n worktree_clean.fish: "(cat /tmp/worktree-clean-test-syntax-err.$fish_pid)
end
rm -f /tmp/worktree-clean-test-syntax-err.$fish_pid

# --- 2. merged candidate offered and removed --------------------------------

set -l s1_root (mktemp -d)
set -l s1_ghq_root $s1_root/ghqroot
set -l s1_repo $s1_ghq_root/example.com/acme/demo.git
set -l s1_origin_scratch (make_bare_repo $s1_repo)
git -C $s1_repo worktree add -q --detach $s1_repo/.worktrees/merged HEAD >/dev/null 2>&1
set -l s1_wt (path resolve $s1_repo/.worktrees/merged)

set -l s1_stub_dir (mktemp -d)
make_fzf_stub $s1_stub_dir

set -l s1_lines (fish -c "
    set -lx PATH $s1_stub_dir \$PATH
    set -lx GHQ_ROOT $s1_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s1_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s1-stderr.$fish_pid)

set -l s1_exit (string replace 'EXIT_STATUS=' '' -- $s1_lines[1])
set -l s1_stderr (cat /tmp/worktree-clean-test-s1-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s1-stderr.$fish_pid

if test "$s1_exit" = 0
    _pass "worktree_clean exits 0 after removing a merged candidate"
else
    _fail "worktree_clean exit status after removing a merged candidate: expected 0, got '$s1_exit'"
end

if not test -d $s1_wt
    _pass "merged worktree directory removed from disk"
else
    _fail "merged worktree directory still exists on disk: $s1_wt"
end

if git -C $s1_repo worktree list --porcelain | string match -q "*$s1_wt*"
    _fail "merged worktree still listed by 'git worktree list' after removal"
else
    _pass "merged worktree no longer listed by 'git worktree list'"
end

if string match -q '*removed 1, skipped 0*' -- $s1_stderr
    _pass "merged-candidate removal prints a 'removed 1, skipped 0' summary"
else
    _fail "merged-candidate removal summary: expected to mention 'removed 1, skipped 0', got '$s1_stderr'"
end

rm -rf $s1_root $s1_stub_dir $s1_origin_scratch

# --- 3. unmerged, recently-touched worktree is never offered ----------------

set -l s2_root (mktemp -d)
set -l s2_ghq_root $s2_root/ghqroot
set -l s2_repo $s2_ghq_root/example.com/acme/demo.git
set -l s2_origin_scratch (make_bare_repo $s2_repo)
git -C $s2_repo worktree add -q --detach $s2_repo/.worktrees/fresh HEAD >/dev/null 2>&1
git -c user.email=t@t -c user.name=t -C $s2_repo/.worktrees/fresh commit -q --allow-empty -m "fresh work"

set -l s2_stub_dir (mktemp -d)
make_fzf_stub $s2_stub_dir

set -l s2_lines (fish -c "
    set -lx PATH $s2_stub_dir \$PATH
    set -lx GHQ_ROOT $s2_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s2_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s2-stderr.$fish_pid)

set -l s2_exit (string replace 'EXIT_STATUS=' '' -- $s2_lines[1])
set -l s2_stderr (cat /tmp/worktree-clean-test-s2-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s2-stderr.$fish_pid

if test "$s2_exit" = 0
    _pass "worktree_clean exits 0 when the only worktree is unmerged and recent"
else
    _fail "worktree_clean exit status with an unmerged, recent worktree: expected 0, got '$s2_exit'"
end

if string match -q '*no stale worktrees found*' -- $s2_stderr
    _pass "unmerged, recently-touched worktree is never offered as a candidate"
else
    _fail "unmerged, recent worktree case stderr: expected to mention 'no stale worktrees found', got '$s2_stderr'"
end

rm -rf $s2_root $s2_stub_dir $s2_origin_scratch

# --- 4. idle-by-time (unmerged) candidate is offered and removed -----------

set -l s3_root (mktemp -d)
set -l s3_ghq_root $s3_root/ghqroot
set -l s3_repo $s3_ghq_root/example.com/acme/demo.git
set -l s3_origin_scratch (make_bare_repo $s3_repo)
git -C $s3_repo worktree add -q --detach $s3_repo/.worktrees/idle HEAD >/dev/null 2>&1
set -l s3_wt (path resolve $s3_repo/.worktrees/idle)
# Idle threshold is 3 days (259200s); back the commit off by an extra hour so
# the comparison isn't sitting right on the boundary.
set -l s3_old_epoch (math (date +%s) - 259200 - 3600)
commit_backdated $s3_wt $s3_old_epoch "old work"

set -l s3_stub_dir (mktemp -d)
make_fzf_stub $s3_stub_dir

set -l s3_lines (fish -c "
    set -lx PATH $s3_stub_dir \$PATH
    set -lx GHQ_ROOT $s3_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s3_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s3-stderr.$fish_pid)

set -l s3_exit (string replace 'EXIT_STATUS=' '' -- $s3_lines[1])
set -l s3_stderr (cat /tmp/worktree-clean-test-s3-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s3-stderr.$fish_pid

if test "$s3_exit" = 0
    _pass "worktree_clean exits 0 after removing an idle-by-time candidate"
else
    _fail "worktree_clean exit status after removing an idle-by-time candidate: expected 0, got '$s3_exit'"
end

if not test -d $s3_wt
    _pass "idle-by-time (unmerged) worktree directory removed from disk"
else
    _fail "idle-by-time worktree directory still exists on disk: $s3_wt"
end

if string match -q '*removed 1, skipped 0*' -- $s3_stderr
    _pass "idle-by-time candidate removal prints a 'removed 1, skipped 0' summary"
else
    _fail "idle-by-time candidate removal summary: expected to mention 'removed 1, skipped 0', got '$s3_stderr'"
end

rm -rf $s3_root $s3_stub_dir $s3_origin_scratch

# --- 5. own current worktree is excluded unconditionally --------------------

set -l s4_root (mktemp -d)
set -l s4_ghq_root $s4_root/ghqroot
set -l s4_repo $s4_ghq_root/example.com/acme/demo.git
# Backdate the repo's own initial commit so the sole worktree, which stays
# at that commit, is simultaneously merged (identical to origin/main) AND
# idle -- both staleness signals fire, and exclusion must still win.
set -l s4_old_epoch (math (date +%s) - 259200 - 3600)
set -l s4_origin_scratch (make_bare_repo $s4_repo $s4_old_epoch)
git -C $s4_repo worktree add -q --detach $s4_repo/.worktrees/own HEAD >/dev/null 2>&1
set -l s4_wt (path resolve $s4_repo/.worktrees/own)

set -l s4_stub_dir (mktemp -d)
make_fzf_stub $s4_stub_dir

set -l s4_lines (fish -c "
    set -lx PATH $s4_stub_dir \$PATH
    set -lx GHQ_ROOT $s4_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s4_wt
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s4-stderr.$fish_pid)

set -l s4_exit (string replace 'EXIT_STATUS=' '' -- $s4_lines[1])
set -l s4_stderr (cat /tmp/worktree-clean-test-s4-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s4-stderr.$fish_pid

if test "$s4_exit" = 0
    _pass "worktree_clean exits 0 when the only worktree is the current one"
else
    _fail "worktree_clean exit status with only the current worktree present: expected 0, got '$s4_exit'"
end

if string match -q '*no stale worktrees found*' -- $s4_stderr
    _pass "the current worktree is excluded even when merged and idle"
else
    _fail "current-worktree exclusion stderr: expected to mention 'no stale worktrees found', got '$s4_stderr'"
end

if test -d $s4_wt
    _pass "current worktree directory still exists on disk"
else
    _fail "current worktree directory was removed from disk: $s4_wt"
end

if git -C $s4_repo worktree list --porcelain | string match -q "*$s4_wt*"
    _pass "current worktree still listed by 'git worktree list'"
else
    _fail "current worktree no longer listed by 'git worktree list' after running worktree_clean from inside it"
end

rm -rf $s4_root $s4_stub_dir $s4_origin_scratch

# --- 6. cancelling the picker performs no deletions --------------------------

set -l s5_root (mktemp -d)
set -l s5_ghq_root $s5_root/ghqroot
set -l s5_repo $s5_ghq_root/example.com/acme/demo.git
set -l s5_origin_scratch (make_bare_repo $s5_repo)
git -C $s5_repo worktree add -q --detach $s5_repo/.worktrees/wt1 HEAD >/dev/null 2>&1
git -C $s5_repo worktree add -q --detach $s5_repo/.worktrees/wt2 HEAD >/dev/null 2>&1
set -l s5_wt1 (path resolve $s5_repo/.worktrees/wt1)
set -l s5_wt2 (path resolve $s5_repo/.worktrees/wt2)

set -l s5_stub_dir (mktemp -d)
make_fzf_stub $s5_stub_dir

set -l s5_lines (fish -c "
    set -lx PATH $s5_stub_dir \$PATH
    set -lx GHQ_ROOT $s5_ghq_root
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s5_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s5-stderr.$fish_pid)

set -l s5_exit (string replace 'EXIT_STATUS=' '' -- $s5_lines[1])
set -l s5_stderr (cat /tmp/worktree-clean-test-s5-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s5-stderr.$fish_pid

if test "$s5_exit" = 0
    _pass "worktree_clean exits 0 when the picker is cancelled"
else
    _fail "worktree_clean exit status on cancel: expected 0, got '$s5_exit'"
end

if test -z "$s5_stderr"
    _pass "cancelling the picker prints no removed/skipped summary"
else
    _fail "cancelling the picker printed unexpected stderr: '$s5_stderr'"
end

if test -d $s5_wt1 -a -d $s5_wt2
    _pass "both candidate worktrees still exist on disk after cancelling"
else
    _fail "a candidate worktree was removed despite cancelling: wt1 exists="(test -d $s5_wt1; and echo yes; or echo no)" wt2 exists="(test -d $s5_wt2; and echo yes; or echo no)
end

rm -rf $s5_root $s5_stub_dir $s5_origin_scratch

# --- 7. a dirty/refused worktree does not abort the rest of the batch -------

set -l s6_root (mktemp -d)
set -l s6_ghq_root $s6_root/ghqroot
set -l s6_repo $s6_ghq_root/example.com/acme/demo.git
set -l s6_origin_scratch (make_bare_repo $s6_repo)
git -C $s6_repo worktree add -q --detach $s6_repo/.worktrees/clean HEAD >/dev/null 2>&1
git -C $s6_repo worktree add -q --detach $s6_repo/.worktrees/dirty HEAD >/dev/null 2>&1
set -l s6_clean_wt (path resolve $s6_repo/.worktrees/clean)
set -l s6_dirty_wt (path resolve $s6_repo/.worktrees/dirty)
echo uncommitted >$s6_dirty_wt/scratch.txt

set -l s6_stub_dir (mktemp -d)
make_fzf_stub $s6_stub_dir

set -l s6_lines (fish -c "
    set -lx PATH $s6_stub_dir \$PATH
    set -lx GHQ_ROOT $s6_ghq_root
    set -lx FZF_STUB_SELECT_INDICES '1 2'
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s6_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s6-stderr.$fish_pid)

set -l s6_exit (string replace 'EXIT_STATUS=' '' -- $s6_lines[1])
set -l s6_stderr (cat /tmp/worktree-clean-test-s6-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s6-stderr.$fish_pid

if test "$s6_exit" = 0
    _pass "worktree_clean exits 0 even when one selected worktree is refused"
else
    _fail "worktree_clean exit status with one refused removal: expected 0, got '$s6_exit'"
end

if not test -d $s6_clean_wt
    _pass "the clean selected worktree was removed"
else
    _fail "the clean selected worktree was not removed: $s6_clean_wt"
end

if test -d $s6_dirty_wt
    _pass "the dirty selected worktree was left in place after git refused to remove it"
else
    _fail "the dirty selected worktree was removed despite having uncommitted changes: $s6_dirty_wt"
end

if string match -q '*removed 1, skipped 1*' -- $s6_stderr
    _pass "mixed batch prints a 'removed 1, skipped 1' summary"
else
    _fail "mixed batch summary: expected to mention 'removed 1, skipped 1', got '$s6_stderr'"
end

rm -rf $s6_root $s6_stub_dir $s6_origin_scratch

# --- 8. scans and removes across every ghq-managed repo, not just one ------

set -l s7_root (mktemp -d)
set -l s7_ghq_root $s7_root/ghqroot
set -l s7_repo_a $s7_ghq_root/example.com/acme/alpha.git
set -l s7_repo_b $s7_ghq_root/example.com/acme/beta.git
set -l s7_origin_scratch_a (make_bare_repo $s7_repo_a)
set -l s7_origin_scratch_b (make_bare_repo $s7_repo_b)
git -C $s7_repo_a worktree add -q --detach $s7_repo_a/.worktrees/wtA HEAD >/dev/null 2>&1
git -C $s7_repo_b worktree add -q --detach $s7_repo_b/.worktrees/wtB HEAD >/dev/null 2>&1
set -l s7_wt_a (path resolve $s7_repo_a/.worktrees/wtA)
set -l s7_wt_b (path resolve $s7_repo_b/.worktrees/wtB)

set -l s7_stub_dir (mktemp -d)
make_fzf_stub $s7_stub_dir

set -l s7_lines (fish -c "
    set -lx PATH $s7_stub_dir \$PATH
    set -lx GHQ_ROOT $s7_ghq_root
    set -lx FZF_STUB_SELECT_INDICES '1 2'
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s7_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s7-stderr.$fish_pid)

set -l s7_exit (string replace 'EXIT_STATUS=' '' -- $s7_lines[1])
set -l s7_stderr (cat /tmp/worktree-clean-test-s7-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s7-stderr.$fish_pid

if test "$s7_exit" = 0
    _pass "worktree_clean exits 0 across a two-repo GHQ_ROOT"
else
    _fail "worktree_clean exit status across a two-repo GHQ_ROOT: expected 0, got '$s7_exit'"
end

if not test -d $s7_wt_a
    _pass "the stale worktree in repo alpha was removed"
else
    _fail "the stale worktree in repo alpha was not removed: $s7_wt_a"
end

if not test -d $s7_wt_b
    _pass "the stale worktree in repo beta was removed"
else
    _fail "the stale worktree in repo beta was not removed: $s7_wt_b"
end

if string match -q '*removed 2, skipped 0*' -- $s7_stderr
    _pass "cross-repo batch prints a 'removed 2, skipped 0' summary"
else
    _fail "cross-repo batch summary: expected to mention 'removed 2, skipped 0', got '$s7_stderr'"
end

rm -rf $s7_root $s7_stub_dir $s7_origin_scratch_a $s7_origin_scratch_b

# --- 9. a worktree that is both merged and idle carries the combined
#        "merged,idle" signal label (own-worktree exclusion must not be what
#        keeps this scenario from reaching the label logic, so this runs
#        from a directory outside the worktree, unlike scenario 5) ----------

set -l s8_root (mktemp -d)
set -l s8_ghq_root $s8_root/ghqroot
set -l s8_repo $s8_ghq_root/example.com/acme/demo.git
# Same technique as scenario 5: backdate the repo's own initial commit so
# the sole worktree, left at that commit, is simultaneously merged
# (identical to origin/main) and idle -- but here we cd elsewhere before
# invoking worktree_clean, so it is never excluded as "own".
set -l s8_old_epoch (math (date +%s) - 259200 - 3600)
set -l s8_origin_scratch (make_bare_repo $s8_repo $s8_old_epoch)
git -C $s8_repo worktree add -q --detach $s8_repo/.worktrees/merged_idle HEAD >/dev/null 2>&1
set -l s8_wt (path resolve $s8_repo/.worktrees/merged_idle)

set -l s8_stub_dir (mktemp -d)
make_fzf_stub $s8_stub_dir

set -l s8_lines (fish -c "
    set -lx PATH $s8_stub_dir \$PATH
    set -lx GHQ_ROOT $s8_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s8_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s8-stderr.$fish_pid)

set -l s8_exit (string replace 'EXIT_STATUS=' '' -- $s8_lines[1])
set -l s8_stderr (cat /tmp/worktree-clean-test-s8-stderr.$fish_pid)
set -l s8_fzf_stdin (cat $s8_stub_dir/fzf-stdin.log 2>/dev/null)
rm -f /tmp/worktree-clean-test-s8-stderr.$fish_pid

if test "$s8_exit" = 0
    _pass "worktree_clean exits 0 after removing a merged-and-idle candidate"
else
    _fail "worktree_clean exit status after removing a merged-and-idle candidate: expected 0, got '$s8_exit'"
end

if string match -q '*merged,idle*' -- $s8_fzf_stdin
    _pass "a candidate that is both merged and idle is offered with the combined 'merged,idle' signal label"
else
    _fail "combined signal label: expected fzf stdin to contain 'merged,idle', got '$s8_fzf_stdin'"
end

if not test -d $s8_wt
    _pass "merged-and-idle worktree directory removed from disk"
else
    _fail "merged-and-idle worktree directory still exists on disk: $s8_wt"
end

rm -rf $s8_root $s8_stub_dir $s8_origin_scratch

# --- 10. idle threshold boundary: exactly at the threshold is NOT stale
#         (unmerged, so only the idle signal is in play) --------------------

set -l s9_root (mktemp -d)
set -l s9_ghq_root $s9_root/ghqroot
set -l s9_repo $s9_ghq_root/example.com/acme/demo.git
set -l s9_origin_scratch (make_bare_repo $s9_repo)
git -C $s9_repo worktree add -q --detach $s9_repo/.worktrees/boundary_at HEAD >/dev/null 2>&1
set -l s9_wt (path resolve $s9_repo/.worktrees/boundary_at)

set -l s9_stub_dir (mktemp -d)
make_fzf_stub $s9_stub_dir

# Capture "now" as late as possible -- after all the slow setup (bare repo
# construction, worktree add) -- so the gap to worktree_clean's own
# `date +%s` (called near the top of the function, before any fetch) stays
# well under a second.
set -l s9_now (date +%s)
set -l s9_epoch (math $s9_now - 259200)
commit_backdated $s9_wt $s9_epoch "boundary at threshold"

set -l s9_lines (fish -c "
    set -lx PATH $s9_stub_dir \$PATH
    set -lx GHQ_ROOT $s9_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s9_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s9-stderr.$fish_pid)

set -l s9_exit (string replace 'EXIT_STATUS=' '' -- $s9_lines[1])
set -l s9_stderr (cat /tmp/worktree-clean-test-s9-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s9-stderr.$fish_pid

if test "$s9_exit" = 0
    _pass "worktree_clean exits 0 when the only worktree sits exactly at the idle threshold"
else
    _fail "worktree_clean exit status at the idle threshold boundary: expected 0, got '$s9_exit'"
end

if string match -q '*no stale worktrees found*' -- $s9_stderr
    _pass "an unmerged worktree exactly at the idle threshold (not past it) is not offered"
else
    _fail "idle-threshold boundary (at threshold) stderr: expected 'no stale worktrees found', got '$s9_stderr'"
end

rm -rf $s9_root $s9_stub_dir $s9_origin_scratch

# --- 11. idle threshold boundary: one second past the threshold IS stale
#         (unmerged, so only the idle signal is in play) --------------------

set -l s10_root (mktemp -d)
set -l s10_ghq_root $s10_root/ghqroot
set -l s10_repo $s10_ghq_root/example.com/acme/demo.git
set -l s10_origin_scratch (make_bare_repo $s10_repo)
git -C $s10_repo worktree add -q --detach $s10_repo/.worktrees/boundary_past HEAD >/dev/null 2>&1
set -l s10_wt (path resolve $s10_repo/.worktrees/boundary_past)

set -l s10_stub_dir (mktemp -d)
make_fzf_stub $s10_stub_dir

set -l s10_now (date +%s)
set -l s10_epoch (math $s10_now - 259200 - 1)
commit_backdated $s10_wt $s10_epoch "boundary past threshold"

set -l s10_lines (fish -c "
    set -lx PATH $s10_stub_dir \$PATH
    set -lx GHQ_ROOT $s10_ghq_root
    set -lx FZF_STUB_SELECT_INDICES 1
    source $func_dir/__fzf_ghq_worktree_paths.fish
    source $func_dir/__fzf_ghq_resolve_default_ref.fish
    source $func_dir/worktree_clean.fish
    cd $s10_root
    worktree_clean
    echo EXIT_STATUS=\$status
" 2>/tmp/worktree-clean-test-s10-stderr.$fish_pid)

set -l s10_exit (string replace 'EXIT_STATUS=' '' -- $s10_lines[1])
set -l s10_stderr (cat /tmp/worktree-clean-test-s10-stderr.$fish_pid)
rm -f /tmp/worktree-clean-test-s10-stderr.$fish_pid

if test "$s10_exit" = 0
    _pass "worktree_clean exits 0 after removing a candidate one second past the idle threshold"
else
    _fail "worktree_clean exit status one second past the idle threshold: expected 0, got '$s10_exit'"
end

if not test -d $s10_wt
    _pass "an unmerged worktree one second past the idle threshold is offered and removed"
else
    _fail "idle-threshold boundary (past threshold) worktree directory still exists on disk: $s10_wt"
end

if string match -q '*removed 1, skipped 0*' -- $s10_stderr
    _pass "idle-threshold-boundary (past threshold) removal prints a 'removed 1, skipped 0' summary"
else
    _fail "idle-threshold boundary (past threshold) summary: expected to mention 'removed 1, skipped 0', got '$s10_stderr'"
end

rm -rf $s10_root $s10_stub_dir $s10_origin_scratch

# --- summary -----------------------------------------------------------------

if test $failures -eq 0
    echo "worktree-clean-test: all checks passed"
    exit 0
else
    echo "worktree-clean-test: $failures check(s) failed"
    exit 1
end
