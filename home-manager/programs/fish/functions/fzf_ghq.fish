function fzf_ghq
    ghq list --full-path | fzf --preview "bat --color=always --style=header,grid --line-range :80 {}/README.*" | read recent
    if not test -n "$recent"
        return
    end

    set session_name (string replace -r '.*/([^/]+)$' '$1' $recent)

    if test -n "$TMUX"
        if tmux has-session -t $session_name 2>/dev/null
            tmux switch-client -t $session_name
        else
            tmux new-session -d -s $session_name -c $recent
            tmux switch-client -t $session_name
        end
    else
        if tmux has-session -t $session_name 2>/dev/null
            tmux attach -t $session_name
        else
            tmux new-session -d -s $session_name -c $recent
            tmux attach -t $session_name
        end
    end

    cd $recent
    commandline -r ''
    commandline -f repaint
end
