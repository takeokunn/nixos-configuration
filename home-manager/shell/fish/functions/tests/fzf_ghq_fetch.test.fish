#!/usr/bin/env fish
# Standalone test script for __fzf_ghq_new_worktree's guarded-fetch step
# (base_ref resolution, lines 84-96 of __fzf_ghq_new_worktree.fish). Run
# with: fish path/to/fzf_ghq_fetch.test.fish
# Never touches the real $HOME or this repo's own .state/: every test runs
# against a scratch $HOME and scratch bare+clone git repos under mktemp.

set -l script_dir (status dirname)
source $script_dir/../__fzf_ghq_resolve_default_ref.fish
source $script_dir/../__fzf_ghq_new_worktree.fish

set -g TESTS_PASSED 0
set -g TESTS_FAILED 0

function pass -a description
    echo "PASS: $description"
    set -g TESTS_PASSED (math $TESTS_PASSED + 1)
end

function fail -a description
    echo "FAIL: $description"
    set -g TESTS_FAILED (math $TESTS_FAILED + 1)
end

# Same scratch shape as fzf_ghq.test.fish's make_scratch_repo, but keeps the
# clone directory around (instead of leaving it to be discarded) so a test
# can push a second, later commit through it to simulate the remote moving
# on after the bare clone was made. Prints "<bare>" then "<clone>" on
# separate lines, so command substitution captures them as two list
# elements.
function make_scratch_repo_pair
    set -l dir (mktemp -d)
    set -l bare "$dir/scratch.git"
    git init --bare -q "$bare"
    git -C "$bare" symbolic-ref HEAD refs/heads/main

    set -l clone "$dir/clone"
    git clone -q "$bare" "$clone" >/dev/null 2>&1
    git -C "$clone" symbolic-ref HEAD refs/heads/main
    echo seed >"$clone/README.md"
    git -C "$clone" add README.md
    git -c user.email=test@example.com -c user.name=test -C "$clone" commit -q -m "initial commit"
    git -C "$clone" push -q origin HEAD:refs/heads/main

    echo $bare
    echo $clone
end

function test_fetch_freshness
    set -lx HOME (mktemp -d)
    set -l pair (make_scratch_repo_pair)
    set -l bare $pair[1]
    set -l clone $pair[2]

    # A `ghq get --bare` clone carries no origin remote at all, so give the
    # scratch bare repo one pointing back at the clone (the opposite
    # direction from a normal clone, but all that matters here is that
    # `git -C $bare fetch origin ...` has somewhere real to fetch from).
    git -C "$bare" remote add origin "$clone"

    # Advance the "remote" clone with a second commit AFTER the bare repo
    # was set up, simulating time passing after `ghq get --bare` ran.
    echo second >"$clone/second.txt"
    git -C "$clone" add second.txt
    git -c user.email=test@example.com -c user.name=test -C "$clone" commit -q -m "second commit"
    set -l new_sha (git -C "$clone" rev-parse HEAD)

    set -l result (__fzf_ghq_new_worktree $bare)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "freshness: __fzf_ghq_new_worktree exits 0"
    else
        fail "freshness: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    set -l worktree_path $result[-1]
    set -l worktree_head (git -C "$worktree_path" rev-parse HEAD 2>/dev/null)

    if test "$worktree_head" = "$new_sha"
        pass "freshness: worktree HEAD is the freshly-fetched commit, not the stale bare-clone-time commit"
    else
        fail "freshness: worktree HEAD (expected '$new_sha', got '$worktree_head')"
    end
end

function test_fetch_failure_fallback
    set -lx HOME (mktemp -d)
    set -l pair (make_scratch_repo_pair)
    set -l bare $pair[1]

    # Point origin at a path that does not exist so the fetch fails outright.
    set -l nonexistent (mktemp -u)
    git -C "$bare" remote add origin "$nonexistent"

    set -l expected_sha (git -C "$bare" rev-parse HEAD)

    set -l result (__fzf_ghq_new_worktree $bare 2>"$TMPDIR/fetch_failure_stderr.$fish_pid")
    set -l exit_code $status
    set -l stderr_output (cat "$TMPDIR/fetch_failure_stderr.$fish_pid")
    rm -f "$TMPDIR/fetch_failure_stderr.$fish_pid"

    if test $exit_code -eq 0
        pass "fetch-failure: __fzf_ghq_new_worktree still exits 0 via local-ref fallback"
    else
        fail "fetch-failure: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    set -l worktree_path $result[-1]
    set -l worktree_head (git -C "$worktree_path" rev-parse HEAD 2>/dev/null)

    if test -n "$worktree_path"; and test -d "$worktree_path"; and test "$worktree_head" = "$expected_sha"
        pass "fetch-failure: produces a valid worktree via the local HEAD-symref fallback"
    else
        fail "fetch-failure: valid worktree via fallback (path='$worktree_path', head='$worktree_head', expected='$expected_sha')"
    end

    if string match -q '*falling back to local refs*' -- $stderr_output
        pass "fetch-failure: 'falling back to local refs' warning printed on stderr"
    else
        fail "fetch-failure: expected 'falling back to local refs' warning on stderr, got: $stderr_output"
    end
end

function test_no_origin_remote_regression
    set -lx HOME (mktemp -d)
    set -l pair (make_scratch_repo_pair)
    set -l bare $pair[1]

    # No origin remote configured on the bare repo at all -- matches
    # fzf_ghq.test.fish's make_scratch_repo shape. The fetch guard must not
    # fire, so behavior must be identical to before this change existed.
    if git -C "$bare" remote get-url origin >/dev/null 2>&1
        fail "no-origin: precondition violated -- scratch bare repo unexpectedly has an origin remote"
        return
    end

    set -l expected_sha (git -C "$bare" rev-parse HEAD)

    set -l result (__fzf_ghq_new_worktree $bare 2>"$TMPDIR/no_origin_stderr.$fish_pid")
    set -l exit_code $status
    set -l stderr_output (cat "$TMPDIR/no_origin_stderr.$fish_pid")
    rm -f "$TMPDIR/no_origin_stderr.$fish_pid"

    if test $exit_code -eq 0
        pass "no-origin: __fzf_ghq_new_worktree exits 0"
    else
        fail "no-origin: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    if string match -q '*fetch*' -- $stderr_output
        fail "no-origin: no fetch-related stderr expected, got: $stderr_output"
    else
        pass "no-origin: no fetch-related stderr printed"
    end

    set -l worktree_path $result[-1]
    set -l worktree_head (git -C "$worktree_path" rev-parse HEAD 2>/dev/null)

    if test -n "$worktree_path"; and test -d "$worktree_path"; and test "$worktree_head" = "$expected_sha"
        pass "no-origin: produces a valid worktree via the HEAD-symref fallback, unchanged from before"
    else
        fail "no-origin: valid worktree via fallback (path='$worktree_path', head='$worktree_head', expected='$expected_sha')"
    end
end

function test_fetch_prunes_deleted_remote_branch
    set -lx HOME (mktemp -d)
    set -l pair (make_scratch_repo_pair)
    set -l bare $pair[1]
    set -l clone $pair[2]

    git -C "$bare" remote add origin "$clone"

    # Create a second branch directly on the clone (no checkout needed --
    # `branch <name> <start-point>` just creates the ref) so the bare repo's
    # first fetch below picks up refs/remotes/origin/feature-doomed
    # alongside refs/remotes/origin/main.
    git -C "$clone" branch -q feature-doomed HEAD

    git -C "$bare" fetch -q origin '+refs/heads/*:refs/remotes/origin/*'
    if git -C "$bare" rev-parse --verify --quiet refs/remotes/origin/feature-doomed >/dev/null 2>&1
        pass "prune: precondition -- refs/remotes/origin/feature-doomed exists after the first fetch"
    else
        fail "prune: precondition violated -- refs/remotes/origin/feature-doomed missing after the first fetch"
        return
    end

    # Simulate the branch being deleted upstream between the bare repo's
    # first fetch and this second worktree-creation call.
    git -C "$clone" branch -q -D feature-doomed

    set -l result (__fzf_ghq_new_worktree $bare)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "prune: __fzf_ghq_new_worktree exits 0"
    else
        fail "prune: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    if git -C "$bare" rev-parse --verify --quiet refs/remotes/origin/feature-doomed >/dev/null 2>&1
        fail "prune: refs/remotes/origin/feature-doomed still present after the guarded fetch -- --prune with the widened refspec did not take effect"
    else
        pass "prune: refs/remotes/origin/feature-doomed is gone after the guarded fetch (--prune + widened refspec both took effect)"
    end

    if git -C "$bare" rev-parse --verify --quiet refs/remotes/origin/main >/dev/null 2>&1
        pass "prune: refs/remotes/origin/main is unaffected by the prune"
    else
        fail "prune: refs/remotes/origin/main unexpectedly missing after the guarded fetch"
    end
end

test_fetch_freshness
test_fetch_failure_fallback
test_no_origin_remote_regression
test_fetch_prunes_deleted_remote_branch

echo ""
echo "Summary: $TESTS_PASSED passed, $TESTS_FAILED failed"

if test $TESTS_FAILED -gt 0
    exit 1
end
exit 0
