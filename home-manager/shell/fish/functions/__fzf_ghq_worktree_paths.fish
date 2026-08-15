# Private helper: list a repo's linked worktree paths, one per line.
# Not named ghq_* so it stays out of the command namespace and off the
# ghq_* glob used elsewhere to manage those functions -- and specifically
# not __ghq_* either, which collides with the vendored fish-ghq NUR plugin's
# own private namespace (e.g. __ghq_repository_search, pulled into the same
# $fish_function_path by home-manager/shell/fish/default.nix). __fzf_ghq_*
# mirrors the existing __fzf_ghq_new_worktree.fish precedent, which inserts
# the same fzf_ buffer for the same reason.
#
#   __fzf_ghq_worktree_paths <repo-path>
#
# <repo-path> is a repo's absolute path (bare or non-bare) as returned by
# `ghq list --full-path`. Prints every entry from
# `git -C <repo-path> worktree list --porcelain` EXCEPT the repo's own
# git-common-dir entry -- a bare repo's porcelain listing includes its own
# bare directory as the first "worktree" entry, and that is never a real
# linked worktree to offer a caller.
#
# The exclusion compares against `git -C <repo-path> rev-parse
# --path-format=absolute --git-common-dir`, NOT <repo-path> verbatim:
# `ghq list --full-path` and git's own porcelain output can disagree after
# symlink resolution (e.g. /tmp vs /private/tmp), which would otherwise leak
# the bare repo itself into the caller's list. This is the canonical pattern
# from fzf_ghq.fish, generalized here since a third caller of the same
# enumeration/exclusion logic (worktree_clean.fish) is the signal this
# repo's own conventions use to justify extraction into a shared helper.
function __fzf_ghq_worktree_paths
    set -l repo_path $argv[1]

    set -l repo_git_dir (git -C $repo_path rev-parse --path-format=absolute --git-common-dir 2>/dev/null)

    for line in (git -C $repo_path worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set -l wt_path (string replace -r '^worktree ' '' -- $line)
            if test "$wt_path" != "$repo_git_dir"
                echo $wt_path
            end
        end
    end
end
