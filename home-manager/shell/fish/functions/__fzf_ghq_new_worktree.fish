# Private helper: create a new worktree under a bare repo's .worktrees/. Not
# named ghq_* to stay out of the ghq_* function glob. Prints the created path
# to stdout; all other messages go to stderr.
function __fzf_ghq_new_worktree
    set -l repo_path $argv[1]
    set -l base_ref $argv[2]
    set -l naming_mode $argv[3]

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

    # Serena never removes an activated project from serena_config.yml, so
    # prune dead entries here before creating the worktree.
    set -l serena_config "$HOME/.serena/serena_config.yml"
    if test -f "$serena_config"
        # Capture the read's own exit status before it can be overwritten --
        # a failed read must not be mistaken for "zero projects" and wipe
        # the list.
        set -l yq_read_output (yq-go eval '.projects[]' "$serena_config" 2>/dev/null)
        set -l yq_read_status $status

        if test $yq_read_status -ne 0
            echo "fzf_ghq: could not read '$serena_config' (yq-go exited $yq_read_status); skipping Serena project prune" >&2
        else
            set -l kept_projects
            for project in $yq_read_output
                test -d "$project"; and set -a kept_projects $project
            end

            # Escape backslashes before quotes so an escaped quote's own
            # backslash isn't re-escaped.
            if test (count $kept_projects) -eq 0
                yq-go eval -i '.projects = []' "$serena_config"
            else
                set -l quoted_projects
                for project in $kept_projects
                    set -a quoted_projects (string replace -a '"' '\\"' -- (string replace -a '\\' '\\\\' -- $project))
                end
                set -l joined_projects (string join '", "' -- $quoted_projects)
                set -l prune_expr ".projects = [\"$joined_projects\"]"
                yq-go eval -i "$prune_expr" "$serena_config"
            end
        end
    end

    # A `ghq get --bare` clone has no refs/remotes/* until something fetches,
    # so origin/main won't resolve without this.
    if test -z "$base_ref"
        # Explicit refspec here (not persisted remote.origin.fetch) avoids
        # leaving refs/remotes/* on the clone permanently.
        if git -C $repo_path remote get-url origin >/dev/null 2>&1
            set -l fetch_output (git -C $repo_path fetch --prune origin '+refs/heads/*:refs/remotes/origin/*' 2>&1)
            if test $status -ne 0
                echo "fzf_ghq: fetch of origin/main failed ($fetch_output); falling back to local refs" >&2
            end
        end
    end
    if test -z "$base_ref"
        set base_ref (__fzf_ghq_resolve_default_ref $repo_path)
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
    set -l base_name
    if test "$naming_mode" = branch
        set -l sanitized_branch_name (string replace -a '/' '-' -- (string replace -r '^origin/' '' -- $base_ref))
        set base_name "$timestamp-$sanitized_branch_name"
    else
        set base_name "$timestamp-$base_sha"
    end
    set -l target_name $base_name
    set -l suffix 2
    while test -e "$worktrees_dir/$target_name"
        set target_name "$base_name-$suffix"
        set suffix (math $suffix + 1)
    end
    set -l target_path "$worktrees_dir/$target_name"

    # Detached, not the branch name: `git worktree add <path> <branch>` fails
    # once a sibling worktree already holds that branch.
    set -l worktree_add_output (git -C $repo_path worktree add --detach $target_path $base_sha 2>&1)
    if test $status -ne 0
        echo "fzf_ghq: failed to create worktree: $worktree_add_output" >&2
        return 1
    end

    # Symlinked (not copied) so all worktrees share Serena memories and
    # .claude settings, except where the checkout already tracks the path --
    # `ln -sfn` would otherwise link *inside* an existing directory instead
    # of failing.
    set -l state_dir "$repo_path/.state"

    # mkdir -p first so the first worktree for a repo bootstraps $state_dir
    # instead of silently skipping the link below.
    mkdir -p "$state_dir/.serena/memories" "$state_dir/.claude"

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

    # Copied, not symlinked: direnv/dotenv tooling expect a real file, and
    # each worktree needs to edit its own env independently.
    for name in .envrc .env
        set -l src "$state_dir/$name"
        set -l dst "$target_path/$name"
        if test -e "$dst"; or test -L "$dst"
            echo "fzf_ghq: $name is provided by the checkout; leaving it un-copied" >&2
            continue
        end
        test -f "$src"; or continue
        cp "$src" "$dst"
        if test "$name" = .envrc
            if command -v direnv >/dev/null 2>&1
                set -l direnv_allow_output (direnv allow "$target_path" 2>&1)
                if test $status -ne 0
                    echo "fzf_ghq: direnv allow failed ($direnv_allow_output)" >&2
                end
            end
        end
    end

    echo "fzf_ghq: created worktree $target_path" >&2
    echo $target_path
end
