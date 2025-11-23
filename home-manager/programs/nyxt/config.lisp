(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :background-color "#282a36"
                         :on-background-color "#f8f8f2"
                         :primary-color "#bd93f9"
                         :on-primary-color "#282a36"
                         :secondary-color "#44475a"
                         :on-secondary-color "#f8f8f2"
                         :accent-color "#ff79c6"
                         :on-accent-color "#282a36"))))

(define-configuration web-buffer
    ((default-modes
      (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))
