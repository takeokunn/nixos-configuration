if test (uname) = "Darwin"
    # for emacs
    set -gx NATIVE_FULL_AOT 1
    set -gx LSP_USE_PLISTS true

    # editor
    set -gx EDITOR "emacs -nw"
    set -gx HOMEBREW_EDITOR "emacs -nw"

    # path
    fish_add_path /usr/local/sbin
    fish_add_path /usr/local/bin
    fish_add_path /opt/homebrew/bin
    fish_add_path /opt/homebrew/sbin
    fish_add_path $HOME/.local/bin

    # alias
    alias cdd 'cd ~/Desktop'
    alias make 'make -j8'
    alias emacs 'emacs -nw'
end
