function fzf_ghq
    set -l preview_cmd 'set -l readme (find {} -maxdepth 1 -iname "README*" 2>/dev/null | head -n1); if test -z "$readme"; set readme (find {}/.worktrees -maxdepth 3 -iname "README*" 2>/dev/null | head -n1); end; if test -n "$readme"; bat --color=always --style=header,grid --line-range :80 $readme; end'
    set -l repo (FZF_TMUX=0 ghq list --full-path | fzf --preview $preview_cmd)
    if test -z "$repo"
        return
    end

    set -l target $repo
    set -l is_bare (git -C $repo rev-parse --is-bare-repository 2>/dev/null)

    if test "$is_bare" = true
        set -l worktree_paths (__fzf_ghq_worktree_paths $repo)

        if test (count $worktree_paths) -eq 0
            set target (__fzf_ghq_new_worktree $repo)
            if test $status -ne 0 -o -z "$target"
                return 1
            end
        else
            set -l wt_choice (printf '%s\n' "[new worktree]" $worktree_paths | fzf --preview "eza --tree --level=2 --color=always {} 2>/dev/null" --prompt "worktree> ")
            if test -z "$wt_choice"
                return
            end

            if test "$wt_choice" = "[new worktree]"
                set target (__fzf_ghq_new_worktree $repo)
                if test $status -ne 0 -o -z "$target"
                    return 1
                end
            else
                set target $wt_choice
            end
        end
    end

    set -l repo_basename (string replace -r '.*/([^/]+)$' '$1' $repo)
    set -l repo_name (string replace -r '\.git$' '' -- $repo_basename)
    set -l session_name (string replace -a '.' '_' -- $repo_name)
    set -l current_session (tmux display-message -p '#S' 2>/dev/null)

    if test -n "$TMUX"
        if test "$session_name" = "$current_session"
            cd $target
        else if tmux has-session -t=$session_name 2>/dev/null
            tmux switch-client -t $session_name
        else
            tmux new-session -d -s $session_name -c $target
            tmux switch-client -t $session_name
        end
    else
        if tmux has-session -t=$session_name 2>/dev/null
            tmux attach -t $session_name
        else
            tmux new-session -s $session_name -c $target
        end
    end
end
