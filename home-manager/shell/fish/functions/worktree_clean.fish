# Sweeps every bare repo's worktrees for ones that are safe to delete --
# merged into the repo's default branch, or idle past a fixed threshold --
# and offers them in a single cross-repo fzf multi-select. Unlike
# worktree_switch/worktree_rm, which act on one repo or one worktree,
# this scans everything ghq knows about in one pass.
function worktree_clean
    # Capture before touching any repo so a worktree removed elsewhere in
    # this loop can never accidentally re-derive this as "current" later.
    set -l own_worktree (git rev-parse --show-toplevel 2>/dev/null)

    set -l idle_threshold_seconds 259200 # 3 days -- user-approved default, not configurable
    set -l now_epoch (date +%s)

    # A bare \t inside double quotes is NOT escape-interpreted by fish (it
    # stays the two literal characters backslash-t) -- only a bare,
    # unquoted \t is. Route through this variable so the picker lines below
    # actually contain a real tab byte, matching the real tab that `fzf
    # --delimiter \t` and `string split -f2 \t` (both unquoted, both real
    # escapes) expect to find in the data.
    set -l tab \t

    set -l candidate_signals
    set -l candidate_paths

    for repo in (ghq list --full-path)
        set -l is_bare (git -C $repo rev-parse --is-bare-repository 2>/dev/null)
        test "$is_bare" = true; or continue

        # Refresh remote-tracking refs before evaluating merged-status, same
        # widened fetch as the worktree-creation path. A failed fetch is not
        # fatal: fall back to whatever local refs this repo already has.
        if git -C $repo remote get-url origin >/dev/null 2>&1
            set -l fetch_output (git -C $repo fetch --prune origin '+refs/heads/*:refs/remotes/origin/*' 2>&1)
            if test $status -ne 0
                echo "worktree_clean: fetch failed for $repo ($fetch_output); using existing local refs" >&2
            end
        end

        # Same three-step fallback chain as __fzf_ghq_new_worktree.fish, now
        # shared via __fzf_ghq_resolve_default_ref.fish rather than replicated.
        set -l default_ref (__fzf_ghq_resolve_default_ref $repo)
        if test -z "$default_ref"
            echo "worktree_clean: cannot resolve a default ref in '$repo'; skipping" >&2
            continue
        end

        # __fzf_ghq_worktree_paths already excludes the bare repo's own entry
        # (comparing against git's own canonical path, not $repo verbatim --
        # mirrors the symlink-resolution caveat documented in
        # fzf_ghq.fish/worktree_switch.fish, e.g. /tmp vs /private/tmp).
        # Also exclude $own_worktree unconditionally here: the invoking
        # shell's own current worktree must never be offered as a deletion
        # candidate, and the helper has no notion of "own worktree" to do
        # that exclusion itself.
        set -l worktree_paths
        for wt in (__fzf_ghq_worktree_paths $repo)
            test "$wt" != "$own_worktree"; and set -a worktree_paths $wt
        end

        for wt in $worktree_paths
            set -l merged false
            git -C $wt merge-base --is-ancestor HEAD $default_ref 2>/dev/null
            test $status -eq 0; and set merged true

            set -l idle false
            set -l last_commit_epoch (git -C $wt log -1 --format=%ct HEAD 2>/dev/null)
            if test -n "$last_commit_epoch"
                if test (math $now_epoch - $last_commit_epoch) -gt $idle_threshold_seconds
                    set idle true
                end
            end

            test "$merged" = true -o "$idle" = true; or continue

            set -l signal
            if test "$merged" = true -a "$idle" = true
                set signal "merged,idle"
            else if test "$merged" = true
                set signal merged
            else
                set signal idle
            end

            set -a candidate_signals $signal
            set -a candidate_paths $wt
        end
    end

    if test (count $candidate_paths) -eq 0
        echo "worktree_clean: no stale worktrees found" >&2
        return 0
    end

    set -l lines
    for i in (seq (count $candidate_paths))
        set -a lines "$candidate_signals[$i]$tab$candidate_paths[$i]"
    end

    set -l selected (printf '%s\n' $lines | fzf --multi --delimiter \t --with-nth 1,2 --preview "eza --tree --level=2 --color=always {2} 2>/dev/null" --prompt "stale worktree(s) to remove> ")
    if test -z "$selected"
        return 0
    end

    set -l removed 0
    set -l skipped 0
    for line in $selected
        set -l path (string split -f2 \t -- $line)
        set -l remove_output (git -C $path worktree remove -- $path 2>&1)
        if test $status -eq 0
            set removed (math $removed + 1)
        else
            echo "worktree_clean: failed to remove $path: $remove_output" >&2
            set skipped (math $skipped + 1)
        end
    end

    echo "worktree_clean: removed $removed, skipped $skipped" >&2
end
