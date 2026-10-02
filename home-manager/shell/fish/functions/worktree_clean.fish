# Sweeps every bare repo's worktrees for ones safe to delete -- merged or
# idle -- and offers them in one cross-repo fzf multi-select.
function worktree_clean
    # Capture before mutating anything, so a worktree removed later in this
    # loop can't be mistaken for "current".
    set -l own_worktree (git rev-parse --show-toplevel 2>/dev/null)
    # A jj workspace has no git toplevel, so fall back to jj's root.
    test -n "$own_worktree"; or set own_worktree (jj workspace root 2>/dev/null)

    set -l idle_threshold_seconds 259200 # 3 days, not configurable
    set -l now_epoch (date +%s)

    # fish doesn't escape-interpret \t inside quotes; route it through this
    # var so the picker lines get a real tab byte.
    set -l tab \t

    set -l candidate_signals
    set -l candidate_paths

    for repo in (ghq list --full-path)
        set -l is_bare (git -C $repo rev-parse --is-bare-repository 2>/dev/null)
        test "$is_bare" = true; or continue

        # Refresh remote-tracking refs before checking merged-status. A
        # failed fetch isn't fatal -- falls back to existing local refs.
        if git -C $repo remote get-url origin >/dev/null 2>&1
            set -l fetch_output (git -C $repo fetch --prune origin '+refs/heads/*:refs/remotes/origin/*' 2>&1)
            if test $status -ne 0
                echo "worktree_clean: fetch failed for $repo ($fetch_output); using existing local refs" >&2
            end
        end

        set -l default_ref (__fzf_ghq_resolve_default_ref $repo)
        if test -z "$default_ref"
            echo "worktree_clean: cannot resolve a default ref in '$repo'; skipping" >&2
            continue
        end

        # Exclude $own_worktree: the invoking shell's own worktree must
        # never be offered as a deletion candidate.
        set -l worktree_paths
        for wt in (__fzf_ghq_worktree_paths $repo)
            test "$wt" != "$own_worktree"; and set -a worktree_paths $wt
        end

        for wt in $worktree_paths
            set -l head_revs HEAD
            set -l last_commit_epoch
            if __fzf_ghq_jj_workspace_p $wt
                # `git -C` here would read the bare repo's own HEAD, so ask jj
                # for the workspace's @. A non-empty @ is unfinished work and
                # yields no revs, so it never counts as merged.
                set -l wc_fields (jj -R $wt log --no-graph -r @ -T 'if(empty, parents.map(|c| c.commit_id()).join(" ")) ++ "\t" ++ committer.timestamp().format("%s")' 2>/dev/null)
                set head_revs (string split --no-empty ' ' -- (string split -f1 \t -- $wc_fields))
                set last_commit_epoch (string split -f2 \t -- $wc_fields)
            else
                set last_commit_epoch (git -C $wt log -1 --format=%ct HEAD 2>/dev/null)
            end

            set -l merged false
            if test (count $head_revs) -gt 0
                set merged true
                for rev in $head_revs
                    git -C $wt merge-base --is-ancestor $rev $default_ref 2>/dev/null; or set merged false
                end
            end

            set -l idle false
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
        set -l remove_output
        set -l remove_status
        if __fzf_ghq_jj_workspace_p $path
            set remove_output (__fzf_ghq_remove_jj_workspace $path)
            set remove_status $status
        else
            set remove_output (git -C $path worktree remove -- $path 2>&1)
            set remove_status $status
        end
        if test $remove_status -eq 0
            set removed (math $removed + 1)
        else
            echo "worktree_clean: failed to remove $path: $remove_output" >&2
            set skipped (math $skipped + 1)
        end
    end

    echo "worktree_clean: removed $removed, skipped $skipped" >&2
end
