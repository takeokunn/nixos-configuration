function fzf_ghq
    set -l recent (FZF_TMUX=0 ghq list --full-path | fzf --preview "bat --color=always --style=header,grid --line-range :80 {}/README.*")
    if test -z "$recent"
        return
    end

    set -l session_name (string replace -r '.*/([^/]+)$' '$1' $recent | string replace -a '.' '_')
    set -l current_session (tmux display-message -p '#S' 2>/dev/null)

    if test -n "$TMUX"
        if test "$session_name" = "$current_session"
            cd $recent
        else if tmux has-session -t=$session_name 2>/dev/null
            tmux switch-client -t $session_name
        else
            tmux new-session -d -s $session_name -c $recent
            tmux switch-client -t $session_name
        end
    else
        if tmux has-session -t=$session_name 2>/dev/null
            tmux attach -t $session_name
        else
            tmux new-session -s $session_name -c $recent
        end
    end
end
