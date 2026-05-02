(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("kakehashi"))
    :activation-fn (lsp-activate-on "markdown")
    :server-id 'kakehashi)))
