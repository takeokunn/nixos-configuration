#!/usr/bin/env fish
# Standalone test script for __fzf_ghq_new_worktree's Serena-pruning and
# .state/ bootstrap behavior. Run with: fish path/to/fzf_ghq.test.fish
# Never touches the real $HOME or this repo's own .state/: every test runs
# against a scratch $HOME and a scratch bare git repo under mktemp.

set -l script_dir (status dirname)
source $script_dir/../fzf_ghq.fish

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

# Builds a scratch bare repo whose HEAD symref resolves via the plain-HEAD
# fallback in __fzf_ghq_new_worktree's base_ref resolution (no origin remote
# exists on a bare repo, so refs/remotes/origin/HEAD and origin/main both
# stay unresolved and the function falls through to `symbolic-ref --short
# HEAD`). Prints the bare repo's absolute path.
function make_scratch_repo
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
end

function test_prune_stale_serena_projects
    set -lx HOME (mktemp -d)
    set -l repo (make_scratch_repo)

    set -l real_dir (mktemp -d)
    set -l missing_dir (mktemp -u)

    mkdir -p "$HOME/.serena"
    printf 'projects:\n  - %s\n  - %s\n' $real_dir $missing_dir >"$HOME/.serena/serena_config.yml"

    set -l result (__fzf_ghq_new_worktree $repo)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "prune: __fzf_ghq_new_worktree exits 0"
    else
        fail "prune: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    # Deliberately not `yq eval '.projects[]' ...`: the `yq` resolved on this
    # machine's PATH is python/kislyuk yq (pkgs.yq via
    # home-manager/shell/advanced.nix), which has no `eval` subcommand at
    # all -- that syntax is mikefarah/yq-go only. Using the same call the
    # implementation uses would make this assertion's oracle share the
    # implementation's own bug instead of independently checking the file.
    set -l remaining (yq -r '.projects[]' "$HOME/.serena/serena_config.yml" 2>/dev/null)

    if contains -- $real_dir $remaining
        pass "prune: existing directory entry retained"
    else
        fail "prune: existing directory entry retained (expected '$real_dir' in [$remaining])"
    end

    if contains -- $missing_dir $remaining
        fail "prune: nonexistent directory entry removed (still present: $missing_dir)"
    else
        pass "prune: nonexistent directory entry removed"
    end
end

function test_prune_skips_write_on_read_failure
    set -lx HOME (mktemp -d)
    set -l repo (make_scratch_repo)

    set -l real_dir (mktemp -d)
    mkdir -p "$HOME/.serena"
    printf 'projects:\n  - %s\n' $real_dir >"$HOME/.serena/serena_config.yml"
    set -l backup (mktemp)
    cp "$HOME/.serena/serena_config.yml" $backup

    # A genuinely malformed YAML file does NOT discriminate old buggy code
    # from the fix: yq-go's `-i` write re-parses the file before writing, so
    # it fails for the same reason the read did, masking the bug (verified
    # empirically -- both old and new code leave a malformed file untouched).
    # Stub `yq-go` ahead of PATH instead: fail only the read invocation (no
    # `-i`) and let the write invocation (`-i`) succeed, so this test proves
    # the code never calls the write at all when the read failed, rather
    # than merely observing that some write happened to also fail.
    set -l stub_dir (mktemp -d)
    set -lx YQ_STUB_WRITE_MARKER (mktemp -u)
    begin
        echo '#!/usr/bin/env fish'
        echo 'if contains -- -i $argv'
        echo '    touch "$YQ_STUB_WRITE_MARKER"'
        echo '    exit 0'
        echo 'else'
        echo '    exit 1'
        echo 'end'
    end >"$stub_dir/yq-go"
    chmod +x "$stub_dir/yq-go"
    set -lx PATH $stub_dir $PATH

    set -l result (__fzf_ghq_new_worktree $repo)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "read-failure: __fzf_ghq_new_worktree exits 0 despite a failed Serena config read"
    else
        fail "read-failure: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    if test -e "$YQ_STUB_WRITE_MARKER"
        fail "read-failure: destructive prune write was invoked despite the read failing"
    else
        pass "read-failure: destructive prune write is skipped when the read fails"
    end

    if cmp -s $backup "$HOME/.serena/serena_config.yml"
        pass "read-failure: serena_config.yml left byte-for-byte unchanged"
    else
        fail "read-failure: serena_config.yml was modified despite a failed read"
    end
end

function test_bootstrap_state_dir
    set -lx HOME (mktemp -d)
    set -l repo (make_scratch_repo)

    if test -d "$repo/.state"
        fail "bootstrap: .state absent before call (unexpectedly already present)"
    else
        pass "bootstrap: .state absent before call"
    end

    set -l result (__fzf_ghq_new_worktree $repo)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "bootstrap: __fzf_ghq_new_worktree exits 0"
    else
        fail "bootstrap: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    set -l worktree_path $result[-1]

    if test -d "$repo/.state/.serena/memories"
        pass "bootstrap: .state/.serena/memories created"
    else
        fail "bootstrap: .state/.serena/memories created"
    end

    if test -d "$repo/.state/.claude"
        pass "bootstrap: .state/.claude created"
    else
        fail "bootstrap: .state/.claude created"
    end

    if test -L "$worktree_path/.serena/memories"
        pass "bootstrap: worktree .serena/memories is symlinked"
    else
        fail "bootstrap: worktree .serena/memories is symlinked (worktree_path='$worktree_path')"
    end

    if test -L "$worktree_path/.claude"
        pass "bootstrap: worktree .claude is symlinked"
    else
        fail "bootstrap: worktree .claude is symlinked (worktree_path='$worktree_path')"
    end
end

function test_missing_serena_config
    set -lx HOME (mktemp -d)
    set -l repo (make_scratch_repo)

    if test -e "$HOME/.serena/serena_config.yml"
        fail "missing-config: serena_config.yml absent before call (unexpectedly already present)"
    else
        pass "missing-config: serena_config.yml absent before call"
    end

    set -l result (__fzf_ghq_new_worktree $repo)
    set -l exit_code $status

    if test $exit_code -eq 0
        pass "missing-config: __fzf_ghq_new_worktree exits 0 when serena_config.yml absent"
    else
        fail "missing-config: __fzf_ghq_new_worktree exit code (expected 0, got $exit_code)"
    end

    set -l worktree_path $result[-1]

    if test -n "$worktree_path"; and test -d "$worktree_path"
        pass "missing-config: produces a valid worktree path"
    else
        fail "missing-config: produces a valid worktree path (got '$worktree_path')"
    end
end

test_prune_stale_serena_projects
test_prune_skips_write_on_read_failure
test_bootstrap_state_dir
test_missing_serena_config

echo ""
echo "Summary: $TESTS_PASSED passed, $TESTS_FAILED failed"

if test $TESTS_FAILED -gt 0
    exit 1
end
exit 0
