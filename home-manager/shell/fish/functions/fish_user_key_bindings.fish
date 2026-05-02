function fish_user_key_bindings
    bind \co fzf_ghq
    bind \c_ edit_command_buffer

    # for vi-mode
    fish_vi_key_bindings insert
    bind -M insert \cf forward-char

    # history search (using native fish history-pager instead of fzf to avoid EINTR in tmux)
    bind \cr history-pager
    bind -M insert \cr history-pager

    # unbind
    bind -e \cg
end
