(define-configuration web-buffer
    ((default-modes
      (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))
