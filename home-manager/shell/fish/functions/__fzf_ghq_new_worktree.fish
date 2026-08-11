# Private helper: create a new worktree under a bare repo's .worktrees/.
# Not named ghq_* so it stays out of the command namespace and off the
# ghq_* glob used elsewhere to manage those functions.
#
#   __fzf_ghq_new_worktree [<repo-path>] [<base-ref>]
#
# Prints the created absolute path to stdout as the ONLY stdout output, so
# callers — fzf_ghq (fzf_ghq.fish) and the public wrapper worktree_new
# (worktree_new.fish) — can consume it directly via command substitution;
# all human-facing messages go to stderr.
function __fzf_ghq_new_worktree
    set -l repo_path $argv[1]
    set -l base_ref $argv[2]

    if test -z "$repo_path"
        set -l git_common_dir (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
        if test -z "$git_common_dir"
            echo "fzf_ghq: not inside a git repository" >&2
            return 1
        end
        set repo_path (string replace -r '/\.git$' '' -- $git_common_dir)
    end

    if not git -C $repo_path rev-parse --git-dir >/dev/null 2>&1
        echo "fzf_ghq: '$repo_path' is not a git repository" >&2
        return 1
    end

    # A repository produced by `ghq get --bare` carries no refs/remotes/* at
    # all and no remote.origin.fetch refspec, so neither refs/remotes/origin/HEAD
    # nor origin/main resolves there — the only ref present is refs/heads/main,
    # which the repository's own HEAD symref points at. The first two candidates
    # only become available once something has written the refspec and fetched.
    if test -z "$base_ref"
        set base_ref (git -C $repo_path symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | string replace 'refs/remotes/' '')
    end
    if test -z "$base_ref"
        if git -C $repo_path rev-parse --verify --quiet origin/main >/dev/null 2>&1
            set base_ref origin/main
        end
    end
    if test -z "$base_ref"
        set base_ref (git -C $repo_path symbolic-ref --short HEAD 2>/dev/null)
    end
    if test -z "$base_ref"
        echo "fzf_ghq: cannot resolve a base ref in '$repo_path'" >&2
        return 1
    end

    set -l base_sha (git -C $repo_path rev-parse --short $base_ref 2>/dev/null)
    if test -z "$base_sha"
        echo "fzf_ghq: cannot resolve base ref '$base_ref' in '$repo_path'" >&2
        return 1
    end

    set -l timestamp (date +%Y%m%dT%H%M%S)
    set -l worktrees_dir "$repo_path/.worktrees"
    set -l base_name "$timestamp-$base_sha"
    set -l target_name $base_name
    set -l suffix 2
    while test -e "$worktrees_dir/$target_name"
        set target_name "$base_name-$suffix"
        set suffix (math $suffix + 1)
    end
    set -l target_path "$worktrees_dir/$target_name"

    # Detach at the resolved commit rather than checking out the branch name.
    # With the HEAD fallback above, base_ref is frequently the bare repo's only
    # branch, and `git worktree add <path> <branch>` fails outright once any
    # sibling worktree already holds it. The directory name records the commit
    # anyway, so the branch name carries nothing extra here.
    set -l worktree_add_output (git -C $repo_path worktree add --detach $target_path $base_sha 2>&1)
    if test $status -ne 0
        echo "fzf_ghq: failed to create worktree: $worktree_add_output" >&2
        return 1
    end

    # A fresh worktree carries no accumulated project knowledge. Serena memories
    # and .claude settings describe the repository rather than any one checkout,
    # so point them at a single copy held beside the bare repo. Linking rather
    # than copying means a memory written in one worktree is visible from the
    # others, and that removing a worktree does not take it away. Serena's cache
    # is deliberately left unlinked: it indexes paths, so sharing it across
    # concurrent worktrees would have them overwrite each other's index.
    #
    # Link only where the checkout does not already provide the path. A
    # repository that tracks .claude has it checked out as a real directory, and
    # `ln -sfn <src> <existing-dir>` silently creates the link *inside* it —
    # yielding .claude/.claude rather than failing. Where the path is tracked the
    # checkout is authoritative, so the shared copy must not shadow it.
    set -l state_dir "$repo_path/.state"
    for pair in ".serena/memories" ".claude"
        set -l src "$state_dir/$pair"
        set -l dst "$target_path/$pair"
        test -d "$src"; or continue
        if test -e "$dst"; or test -L "$dst"
            echo "fzf_ghq: $pair is provided by the checkout; leaving it unlinked" >&2
            continue
        end
        mkdir -p (dirname "$dst")
        ln -sfn "$src" "$dst"
    end

    echo "fzf_ghq: created worktree $target_path" >&2
    echo $target_path
end
