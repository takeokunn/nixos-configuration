#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

TEMP_FILE=$(mktemp)

kitty \
  --config ~/.config/kitty/emacsclient_transparent.conf \
  --title "Temporary Editor" \
  sh -c "
emacsclient -c --eval \
  \"(progn
     (setq initial-buffer-choice nil)
     (let ((file \"$TEMP_FILE\"))
       (find-file file)
       (add-hook 'after-save-hook
                 (lambda ()
                   (when (eq (buffer-file-name) file)
                     (with-current-buffer (current-buffer)
                       (shell-command-on-region (point-min) (point-max) \"wl-copy\"))))
                 nil 'local)
       (add-hook 'kill-buffer-hook
                 (lambda ()
                   (when (eq (buffer-file-name) file)
                     (delete-file file)
                     (message \"Temporary file deleted: %s\" file)
                     (delete-frame))))
       ))
   \"
"
