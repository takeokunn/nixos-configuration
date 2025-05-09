function fzf_ghq
    ghq list --full-path | fzf --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/README.*" | read recent
    if [ $recent ]
        cd $recent
        commandline -r ''
        commandline -f repaint
    end
end
