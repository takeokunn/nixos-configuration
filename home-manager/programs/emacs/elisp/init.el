;; init.el --- My init.el -*- lexical-binding: t -*-
;; Configurations for Emacs
;;                                         Takeo Obara  <bararararatty@gmail.com>

(defconst my/enable-profile t
  "If true, enable profile")

(defmacro when-darwin (&rest body)
  (when (string= system-type "darwin")
    `(progn ,@body)))

(defmacro when-darwin-not-window-system (&rest body)
  (when (and (string= system-type "darwin")
             window-system)
    `(progn ,@body)))

(setq user-full-name "takeokunn")
(setq user-mail-address "bararararatty@gmail.com")

(when my/enable-profile
  (require 'profiler)
  (profiler-start 'cpu))

(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defconst my/before-load-init-time (current-time))

;;;###autoload
(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defvar my/tick-previous-time my/before-load-init-time)

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my/emacs-init-time)

(defun autoload-if-found (functions file &optional docstring interactive type)
  "Set autoload iff FILE has found."
  (when (locate-library file)
    (dolist (f functions)
      (autoload f file docstring interactive type))))

(eval-and-compile
  (setq byte-compile-warnings '(cl-functions))
  (require 'cl-lib nil t))

(defun my/disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'my/disable-show-trailing-whitespace))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'my/disable-show-trailing-whitespace))

(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook #'my/disable-show-trailing-whitespace))

(with-eval-after-load 'minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'my/disable-show-trailing-whitespace))

(with-eval-after-load 'dashboard
  (add-hook 'dashboard-mode-hook #'my/disable-show-trailing-whitespace))

(with-eval-after-load 'simple
  (add-hook 'fundamental-mode-hook #'my/disable-show-trailing-whitespace))

(autoload-if-found '(global-display-line-numbers-mode) "display-line-numbers" nil t)

(add-hook 'window-setup-hook #'global-display-line-numbers-mode)

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-grow-only t))

(with-eval-after-load 'simple
  (setq kill-whole-line t))

(add-hook 'window-setup-hook #'show-paren-mode)

(with-eval-after-load 'paren
  (setq show-paren-style 'mixed))

(add-hook 'window-setup-hook #'electric-pair-mode)

;; language and locale
(setq system-time-locale "C")

;; coding system
(add-hook 'window-setup-hook
          #'(lambda ()
              (set-default-coding-systems 'utf-8-unix)
              (prefer-coding-system 'utf-8-unix)
              (set-selection-coding-system 'utf-8-unix)))

;; prefer-coding-system take effect equally to follows
(setq locale-coding-system 'utf-8-unix)
(add-hook 'window-setup-hook
          #'(lambda ()
              (set-buffer-file-coding-system 'utf-8-unix)
              (set-file-name-coding-system 'utf-8-unix)
              (set-terminal-coding-system 'utf-8-unix)
              (set-keyboard-coding-system 'utf-8-unix)))

(add-hook 'window-setup-hook #'global-auto-revert-mode)

(add-hook 'window-setup-hook
          #'(lambda ()
              (fset 'yes-or-no-p 'y-or-n-p)))

(keymap-global-set "M-¥" #'(lambda () (interactive) (insert "\\")))
(keymap-global-set "C-a" #'back-to-indentation)
(keymap-global-set "C-z" nil)
(keymap-global-set "C-;" #'comment-dwim)
(keymap-global-set "C-M-/" #'undo-redo)
(keymap-global-set "C-c i" #'find-function)
(keymap-global-set "C-c C-o" #'org-open-at-point)
(keymap-global-set "C-x C-o" #'other-window)
(keymap-global-set "M-h" #'backward-kill-word)

(keymap-global-set "C-x l" 'next-buffer)
(keymap-global-set "C-x h" 'previous-buffer)
(keymap-global-set "C-x C-b" #'switch-to-buffer)

(keymap-global-set "C-x C-k" nil)

(when window-system
  (keymap-global-set "C-x C-c" nil))

(define-key minibuffer-mode-map (kbd "C-h") #'delete-backward-char)
(define-key minibuffer-mode-map (kbd "M-h") #'backward-kill-word)
(define-key minibuffer-mode-map (kbd "C-j") #'exit-minibuffer)
(define-key minibuffer-mode-map (kbd "M-RET") #'exit-minibuffer)

(add-hook 'window-setup-hook #'savehist-mode)

(defun my/copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun my/paste-to-osx (text)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when-darwin-not-window-system
 (setq interprogram-cut-function #'my/paste-to-osx)
 (setq interprogram-paste-function #'my/copy-from-osx))

(with-eval-after-load 'comp-run
  (setq native-comp-async-jobs-number 8)
  (setq native-comp-speed 2)
  (setq native-comp-always-compile t))

(with-eval-after-load 'warnings
  (setq warning-suppress-types '((comp))))

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(add-hook 'window-setup-hook
          #'(lambda ()
              (with-current-buffer "*scratch*"
                (emacs-lock-mode 'kill))
              (with-current-buffer "*Messages*"
                (emacs-lock-mode 'kill))))

(with-eval-after-load 'time
  (setq display-time-24hr-format t))

(with-eval-after-load 'warnings
  (setq warning-minimum-level :error))

(defconst my/enable-warning-log nil)

(defun set-fontset-font:around (set-fontset-font name target font-spec &optional frame add)
  "Warn if specified font is not installed."
  (if (stringp font-spec)
      (setq font-spec (font-spec :family font-spec)))
  (if (and (fontp font-spec)
           (null (find-font font-spec))
           my/enable-warning-log)
      (warn "set-fontset-font: font %s is not found." (font-get font-spec :family))
    (ignore-errors
      (funcall set-fontset-font name target font-spec frame add))))

;; reset all settings in default fontset
(add-hook 'window-setup-hook
          #'(lambda ()
              (advice-add 'set-fontset-font :around #'set-fontset-font:around)

              (when (functionp 'set-fontset-font)
                (if (find-font (font-spec :family "Noto Sans"))
                    (set-fontset-font t '(0 . #x3fffff) "Noto Sans"))

                ;; multiple platform
                (set-fontset-font t 'latin "Noto Sans")
                (set-fontset-font t 'greek "Noto Sans")
                (set-fontset-font t 'phonetic "Noto Sans")
                (set-fontset-font t 'coptic "Noto Sans Coptic")
                (set-fontset-font t 'coptic "Noto Sans Symbols2" nil 'append)
                (set-fontset-font t 'cyrillic "Noto Sans")
                (set-fontset-font t 'armenian "Noto Sans Armenian")
                (set-fontset-font t 'hebrew "Noto Sans Hebrew")
                (set-fontset-font t 'arabic "Noto Sans Arabic")
                (set-fontset-font t 'syriac "Noto Sans Syriac")
                (set-fontset-font t 'thaana "Noto Sans Thaana")
                (set-fontset-font t 'nko "Noto Sans N'Ko")
                (set-fontset-font t 'samaritan "Noto Sans Samaritan")
                (set-fontset-font t 'mandaic "Noto Sans Mandaic")
                (set-fontset-font t 'devanagari "Noto Sans Devanagari")
                (set-fontset-font t 'bengali "Noto Sans Bengali")
                (set-fontset-font t 'gurmukhi "Noto Sans Gurmukhi")
                (set-fontset-font t 'gujarati "Noto Sans Gujanrati")
                (set-fontset-font t 'oriya "Noto Sans Oriya")
                (set-fontset-font t 'tamil "Noto Sans Tamil")
                (set-fontset-font t 'tamil "Noto Sans Tamil Supplement" nil 'append)
                (set-fontset-font t 'telugu "Noto Sans Telugu")
                (set-fontset-font t 'kannada "Noto Sans Kannada")
                (set-fontset-font t 'malayalam "Noto Sans Malayalam")
                (set-fontset-font t 'sinhala "Noto Sans Sinhala")
                (set-fontset-font t 'thai "Noto Sans Thai")
                (set-fontset-font t 'lao "Noto Sans Lao")
                (set-fontset-font t 'tibetan "Noto Sans Tibetan")
                (set-fontset-font t 'burmese "Noto Sans Myanmar")
                (set-fontset-font t 'georgian "Noto Sans Georgian")
                (set-fontset-font t 'hangul "Noto Sans CJK KR")
                (set-fontset-font t 'ethiopic "Noto Sans Ethiopic")
                (set-fontset-font t 'cherokee "Noto Sans Cherokee")
                (set-fontset-font t 'canadian-aboriginal "Noto Sans Canadian Aboriginal")
                (set-fontset-font t 'ogham "Noto Sans Ogham")
                (set-fontset-font t 'runic "Noto Sans Runic")
                (set-fontset-font t 'tagalog "Noto Sans Tagalog")
                (set-fontset-font t 'hanunoo "Noto Sans Hanunoo")
                (set-fontset-font t 'buhid "Noto Sans Buhid")
                (set-fontset-font t 'tagbanwa "Noto Sans Tagbanwa")
                (set-fontset-font t 'khmer "Noto Sans Khmer")
                (set-fontset-font t 'mongolian "Noto Sans Mongolian")
                (set-fontset-font t 'limbu "Noto Sans Limbu")
                (set-fontset-font t 'tai-le "Noto Sans Tai Le")
                (set-fontset-font t 'tai-lue "Noto Sans NewTaiLue")
                (set-fontset-font t 'buginese "Noto Sans Buginese")
                (set-fontset-font t 'tai-tham "Noto Sans Tai Tham")
                (set-fontset-font t 'balinese "Noto Sans Balinese")
                (set-fontset-font t 'sundanese "Noto Sans Sundanese")
                (set-fontset-font t 'vedic "Noto Sans Devanagari")
                (set-fontset-font t 'symbol "Noto Sans CJK JP")
                (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
                (set-fontset-font t 'symbol "Noto Sans" nil 'append)
                (set-fontset-font t 'symbol "Noto Sans Math" nil 'append)
                (set-fontset-font t 'symbol "Noto Emoji" nil 'append)
                (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
                (set-fontset-font t 'braille "Noto Sans Symbols2")
                (set-fontset-font t 'batak "Noto Sans Batak")
                (set-fontset-font t 'lepcha "Noto Sans Lepcha")
                (set-fontset-font t 'ol-chiki "Noto Sans Ol Chiki")
                (set-fontset-font t 'glagolitic "Noto Sans Glagolitic")
                (set-fontset-font t 'tifinagh "Noto Sans Tifinagh")
                (set-fontset-font t 'han "Noto Sans CJK JP")
                (set-fontset-font t 'ideographic-description "Noto Sans CJK JP")
                (set-fontset-font t 'cjk-misc "Noto Sans CJK JP")
                (set-fontset-font t 'kana "Noto Sans CJK JP")
                (set-fontset-font t 'bopomofo "Noto Sans CJK TC")
                (set-fontset-font t 'kanbun "Noto Sans CJK JP")
                (set-fontset-font t 'yi "Noto Sans Yi")
                (set-fontset-font t 'lisu "Noto Sans Lisu")
                (set-fontset-font t 'vai "Noto Sans Vai")
                (set-fontset-font t 'bamum "Noto Sans Bamum")
                (set-fontset-font t 'syloti-nagri "Noto Sans Syloti Nagri")
                (set-fontset-font t 'north-indic-number "Noto Sans Devanagari")
                (set-fontset-font t 'phags-pa "Noto Sans Phags Pa")
                (set-fontset-font t 'saurashtra "Noto Sans Saurashtra")
                (set-fontset-font t 'kayah-li "Noto Sans Kayah Li")
                (set-fontset-font t 'rejang "Noto Sans Rejang")
                (set-fontset-font t 'javanese "Noto Sans Javanese")
                (set-fontset-font t 'cham "Noto Sans Cham")
                (set-fontset-font t 'tai-viet "Noto Sans Tai Viet")
                (set-fontset-font t 'meetei-mayek "Noto Sans Meetei Mayek")
                (set-fontset-font t 'vertical-form "Noto Sans CJK JP")
                (set-fontset-font t '(#xfe50 . #xfe6b) "Noto Sans CJK JP") ; symbol
                (set-fontset-font t '(#xfff9 . #xfffb) "Noto Sans Symbols2") ; nil
                (set-fontset-font t 'linear-b "Noto Sans Linear B")
                (set-fontset-font t 'aegean-number "Noto Sans Linear B")
                (set-fontset-font t 'ancient-greek-number "Noto Sans Symbols2")
                (set-fontset-font t 'ancient-symbol "Noto Sans Symbols2")
                (set-fontset-font t 'phaistos-disc "Noto Sans Symbols2")
                (set-fontset-font t 'lycian "Noto Sans Lycian")
                (set-fontset-font t 'carian "Noto Sans Carian")
                (set-fontset-font t 'old-italic "Noto Sans Old Italic")
                (set-fontset-font t 'gothic "Noto Sans Gothic")
                (set-fontset-font t 'old-permic "Noto Sans Old Permic")
                (set-fontset-font t 'ugaritic "Noto Sans Ugaritic")
                (set-fontset-font t 'old-persian "Noto Sans OldPersian")
                (set-fontset-font t 'deseret "Noto Sans Deseret")
                (set-fontset-font t 'shavian "Noto Sans Shavian")
                (set-fontset-font t 'osmanya "Noto Sans Osmanya")
                (set-fontset-font t 'osage "Noto Sans Osage")
                (set-fontset-font t 'elbasan "Noto Sans Elbasan")
                (set-fontset-font t 'caucasian-albanian "Noto Sans CaucAlban")
                (set-fontset-font t 'linear-a "Noto Sans Linear A")
                (set-fontset-font t 'cypriot-syllabary "Noto Sans Cypriot")
                (set-fontset-font t 'aramaic "Noto Sans ImpAramaic")
                (set-fontset-font t 'palmyrene "Noto Sans Palmyrene")
                (set-fontset-font t 'nabataean "Noto Sans Nabataean")
                (set-fontset-font t 'hatran "Noto Sans Hatran")
                (set-fontset-font t 'phoenician "Noto Sans Phoenician")
                (set-fontset-font t 'lydian "Noto Sans Lydian")
                (set-fontset-font t 'meroitic "Noto Sans Meroitic")
                (set-fontset-font t 'kharoshthi "Noto Sans Kharoshthi")
                (set-fontset-font t 'old-south-arabian "Noto Sans OldSouArab")
                (set-fontset-font t 'old-north-arabian "Noto Sans OldNorArab")
                (set-fontset-font t 'manichaean "Noto Sans Manichaean")
                (set-fontset-font t 'avestan "Noto Sans Avestan")
                (set-fontset-font t 'inscriptional-parthian "Noto Sans Inscriptional Parthian")
                (set-fontset-font t 'inscriptional-pahlavi "Noto Sans Inscriptional Pahlavi")
                (set-fontset-font t 'psalter-pahlavi "Noto Sans PsaPahlavi")
                (set-fontset-font t 'old-turkic "Noto Sans Old Turkic")
                (set-fontset-font t 'old-hungarian "Noto Sans OldHung")
                (set-fontset-font t 'hanifi-rohingya "Noto Sans HanifiRohg")
                (set-fontset-font t 'rumi-number "Noto Sans Symbols2")
                (set-fontset-font t 'old-sogdian "Noto Sans OldSogdian")
                (set-fontset-font t 'sogdian "Noto Sans Sogdian")
                (set-fontset-font t 'elymaic "Noto Sans Elymaic")
                (set-fontset-font t 'brahmi "Noto Sans Brahmi")
                (set-fontset-font t 'kaithi "Noto Sans Kaithi")
                (set-fontset-font t 'sora-sompeng "Noto Sans SoraSomp")
                (set-fontset-font t 'chakma "Noto Sans Chakma")
                (set-fontset-font t 'mahajani "Noto Sans Mahajani")
                (set-fontset-font t 'sharada "Noto Sans Sharada")
                (set-fontset-font t 'sinhala-archaic-number "Noto Sans Sinhala")
                (set-fontset-font t 'khojki "Noto Sans Khojki")
                (set-fontset-font t 'multani "Noto Sans Multani")
                (set-fontset-font t 'khudawadi "Noto Sans Khudawadi")
                (set-fontset-font t 'grantha "Noto Sans Grantha")
                (set-fontset-font t 'newa "Noto Sans Newa")
                (set-fontset-font t 'tirhuta "Noto Sans Tirhuta")
                (set-fontset-font t 'siddham "Noto Sans Siddham")
                (set-fontset-font t 'modi "Noto Sans Modi")
                (set-fontset-font t 'takri "Noto Sans Takri")
                (set-fontset-font t 'ahom "Noto Serif Ahom")
                (set-fontset-font t 'dogra "Noto Serif Dogra")
                (set-fontset-font t 'warang-citi "Noto Sans WarangCiti")
                (set-fontset-font t 'zanabazar-square "Noto Sans Zanabazar")
                (set-fontset-font t 'soyombo "Noto Sans Soyombo")
                (set-fontset-font t 'pau-cin-hau "Noto Sans PauCinHau")
                (set-fontset-font t 'bhaiksuki "Noto Sans Bhaiksuki")
                (set-fontset-font t 'marchen "Noto Sans Marchen")
                (set-fontset-font t 'masaram-gondi "Noto Sans Masaram Gondi")
                (set-fontset-font t 'gunjala-gondi "Noto Sans Gunjala Gondi")
                (set-fontset-font t 'cuneiform "Noto Sans Cuneiform")
                (set-fontset-font t 'cuneiform-numbers-and-punctuation "Noto Sans Cuneiform")
                (set-fontset-font t 'egyptian "Noto Sans EgyptHiero")
                (set-fontset-font t 'anatolian "Noto Sans AnatoHiero")
                (set-fontset-font t 'mro "Noto Sans Mro")
                (set-fontset-font t 'bassa-vah "Noto Sans Bassa Vah")
                (set-fontset-font t 'pahawh-hmong "Noto Sans Pahawh Hmong")
                (set-fontset-font t 'miao "Noto Sans Miao")
                (set-fontset-font t 'tangut "Noto Serif Tangut")
                (set-fontset-font t 'tangut-components "Noto Serif Tangut")
                (set-fontset-font t '(#x16fe0 . #x16fe0) "Noto Serif Tangut")
                (set-fontset-font t 'duployan-shorthand "Noto Sans Duployan")
                (set-fontset-font t 'byzantine-musical-symbol "Noto Music")
                (set-fontset-font t 'musical-symbol "Noto Music")
                (set-fontset-font t 'ancient-greek-musical-notation "Noto Music")
                (set-fontset-font t 'mayan-numeral "Noto Sans Mayan Numerals")
                (set-fontset-font t 'tai-xuan-jing-symbol "Noto Sans Symbols2")
                (set-fontset-font t 'counting-rod-numeral "Noto Sans Symbols2")
                (set-fontset-font t 'mathematical "Noto Sans Math")
                (set-fontset-font t 'wancho "Noto Sans Wancho")
                (set-fontset-font t 'mende-kikakui "Noto Sans Mende Kikakui")
                (set-fontset-font t 'adlam "Noto Sans Adlam")
                (set-fontset-font t 'indic-siyaq-number "Noto Sans Indic Siyaq Numbers")
                (set-fontset-font t '(#x1ee00 . #x1eeff) "Noto Sans Math") ; arabic
                (set-fontset-font t 'mahjong-tile "Noto Sans Symbols2")
                (set-fontset-font t 'domino-tile "Noto Sans Symbols2")
                (set-fontset-font t 'playing-cards "Noto Sans Symbols2")

                ;; non Noto fonts
                (set-fontset-font t 'kana "UniHentaiKana" nil 'append)
                (set-fontset-font t 'latin "Iosevka" nil 'append)
                (set-fontset-font t 'symbol "Iosevka" nil 'append)

                ;; Nerd Font (defined thru -#xfd46)
                (set-fontset-font t '( #xe000 .  #xf136) "Inconsolata Nerd Font"))))

(setq echo-keystrokes 0.1)

(setq enable-recursive-minibuffers t)

(setq inhibit-compacting-font-caches t)

(add-hook 'window-setup-hook #'save-place-mode)

(setq enable-local-variables :all)

(with-eval-after-load 'password-cache
  (setq password-cache t)
  (setq password-cache-expiry 3600))

(setq tab-width 4)

(add-hook 'window-setup-hook
          #'(lambda ()
              (indent-tabs-mode nil)))

(with-eval-after-load 'minibuffer
  (setq read-file-name-completion-ignore-case t))

(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

(autoload-if-found '(apache-mode) "apache-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.htaccess$" . apache-mode))

(autoload-if-found '(bazel-mode) "bazel" nil t)

(autoload-if-found '(bison-mode flex-mode jison-mode) "bison-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.jison\\'" . jison-mode))

(autoload-if-found '(cask-mode) "cask-mode" nil t)

(add-to-list 'auto-mode-alist '("/Cask\\'" . cask-mode))

(autoload-if-found '(cfn-mode) "cfn-mode" nil t)
;; (autoload-if-found '(flycheck-cfn-setup) "flycheck-cfn" nil t)

(add-to-list 'magic-mode-alist '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-mode))

;; (with-eval-after-load 'cfn-mode
;;   (add-hook 'cfn-mode-hook #'flycheck-cfn-setup))

(autoload-if-found '(clojure-mode clojurescript-mode) "clojure-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(with-eval-after-load 'clojure-mode
  ;; config
  (setq clojure-toplevel-inside-comment-form t))

(autoload-if-found '(cmake-mode) "cmake-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

(autoload-if-found '(coffee-mode) "coffee-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))
(add-to-list 'auto-mode-alist '("yabairc$" . conf-mode))
(add-to-list 'auto-mode-alist '("skhdrc$" . conf-mode))

(autoload-if-found '(crontab-mode) "crontab-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(autoload-if-found '(csharp-mode) "csharp-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(autoload-if-found '(csv-mode) "csv-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

(autoload-if-found '(cuda-mode) "cuda-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))

(autoload-if-found '(crystal-mode) "crystal-mode" nil t)

(add-to-list 'auto-mode-alist '("Projectfile$" . crystal-mode))
(add-to-list 'auto-mode-alist
             (cons (purecopy (concat "\\(?:\\."
                                     "cr"
                                     "\\)\\'")) 'crystal-mode))

(autoload-if-found '(dart-mode) "dart-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.dart$" . dart-mode))

(autoload-if-found '(dhall-mode) "dhall-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.dhall$" . dhall-mode))

(autoload-if-found '(direnv-mode direnv-envrc-mode) "direnv" nil t)

(add-to-list 'auto-mode-alist '("\\.envrc" . direnv-envrc-mode))

(autoload-if-found '(docker-compose-mode) "docker-compose-mode" nil t)

(add-to-list 'auto-mode-alist '("\\docker-compose*" . docker-compose-mode))

(autoload-if-found '(dockerfile-mode) "dockerfile-mode" nil t)

(add-to-list 'auto-mode-alist '("\\Dockerfile$" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile_Ecs$" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile_EcsDeploy" . dockerfile-mode))

(with-eval-after-load 'dockerfile-mode
  (add-hook 'dockerfile-mode-hook #'flycheck-mode))

(autoload-if-found '(dotenv-mode) "dotenv-mode" nil t)

(add-to-list 'auto-mode-alist '(".env" . dotenv-mode))
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(autoload-if-found '(elixir-mode) "elixir-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.elixir$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.ex$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs$" . elixir-mode))
(add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode))

(autoload-if-found '(elm-mode) "elm-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))

(add-to-list 'auto-mode-alist '("Keg" . emacs-lisp-mode))

(autoload-if-found '(fish-mode) "fish-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.fish$" . fish-mode))

(with-eval-after-load 'fish-mode
  (setq fish-enable-auto-indent t))

(autoload-if-found '(forth-mode) "forth-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.f$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.forth$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.4th$" . forth-mode))

(autoload-if-found '(f90-mode) "f90" nil t)

(add-to-list 'auto-mode-alist '("\\.f\\(y90\\|y?pp\\)\\'" . f90-mode))

(autoload-if-found '(fsharp-mode) "fsharp-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))

(autoload-if-found '(gitignore-mode gitconfig-mode gitattributes-mode) "git-modes" nil t)

;; gitignore-mode
(add-to-list 'auto-mode-alist '("\\.dockerignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.prettierignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("/git/ignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("/git/ignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("CODEOWNERS" . gitignore-mode))

;; gitconfig-mode
(add-to-list 'auto-mode-alist '("\\.git-pr-release$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("/\\.git/config\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("/modules/.*/config\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("/git/config\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("/\\.gitmodules\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("/etc/gitconfig\\'" . gitconfig-mode))

;; gitattributes
(add-to-list 'auto-mode-alist '("/\\.gitattributes\\'" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("\.gitattributes$" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("/info/attributes\\'" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("/git/attributes\\'" . gitattributes-mode))

(autoload-if-found '(glsl-mode) "glsl-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.vsh$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fsh$" . glsl-mode))

(autoload-if-found '(go-mode) "go-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("^go.mod$" . go-mode))

(with-eval-after-load 'go-mode
  ;; config
  (setq gofmt-command "goimports")

  ;; hook
  (add-hook 'before-save-hook #'gofmt-before-save))

(autoload-if-found '(gradle-mode) "gradle-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.gradle$" . gradle-mode))

(autoload-if-found '(graphql-mode) "graphql-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))

(with-eval-after-load 'graphql-mode
  (setq graphql-indent-level 4))

(autoload-if-found '(graphviz-dot-mode) "graphviz-dot-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

(with-eval-after-load 'graphviz-dot-mode
  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 2))

(autoload-if-found '(groovy-mode) "groovy-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("/Jenkinsfile\\'" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(autoload-if-found '(hack-mode) "hack-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.hack$" . hack-mode))
(add-to-list 'auto-mode-alist '("\\.hck$" . hack-mode))
(add-to-list 'auto-mode-alist '("\\.hhi$" . hack-mode))

(autoload-if-found '(haskell-mode) "haskell-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cable$" . haskell-mode))

(autoload-if-found '(hcl-mode) "hcl-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.hcl$" . hcl-mode))

(autoload-if-found '(hy-mode) "hy-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.hy$" . hy-mode))

(autoload-if-found '(ini-mode) "ini-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.ini$" . ini-mode))

(autoload-if-found '(jade-mode) "jade-mode" nil t)
(autoload-if-found '(stylus-mode) "stylus-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))

(autoload-if-found '(java-mode) "java-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

(autoload-if-found '(js2-mode) "js2-mode" nil t)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js2-mode))

(with-eval-after-load 'js2-mode
  ;; config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil))

(autoload-if-found '(json-mode) "json-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.textlintrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.prettierrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.markuplintrc$" . json-mode))

(with-eval-after-load 'json-mode
  (add-hook 'json-mode-hook #'flycheck-mode))

(autoload-if-found '(jsonnet-mode
                     jsonnet-eval-buffer
                     jsonnet-jump
                     jsonnet-reformat-buffer) "jsonnet-mode" nil t)

(add-to-list 'auto-mode-alist (cons "\\.jsonnet\\'" 'jsonnet-mode))
(add-to-list 'auto-mode-alist (cons "\\.libsonnet\\'" 'jsonnet-mode))

(with-eval-after-load 'jsonnet-mode
  ;; config
  (setq jsonnet-indent-level 4)

  ;; keybind
  (define-key jsonnet-mode-map (kbd "C-c C-c") #'jsonnet-eval-buffer)
  (define-key jsonnet-mode-map (kbd "C-c C-f") #'jsonnet-jump)
  (define-key jsonnet-mode-map (kbd "C-c C-r") #'jsonnet-reformat-buffer))

(autoload-if-found '(kotlin-mode) "kotlin-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode))

(autoload-if-found '(lisp-mode) "lisp-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.lemrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

;; (autoload-if-found '(lua-mode) "lua-mode" nil t)

;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))

(autoload-if-found '(markdown-mode) "markdown-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(with-eval-after-load 'markdown-mode
  ;; config
  (setq markdown-code-lang-modes (append '(("diff" . diff-mode)
                                             ("hs" . haskell-mode)
                                             ("html" . web-mode)
                                             ("ini" . conf-mode)
                                             ("js" . web-mode)
                                             ("jsx" . web-mode)
                                             ("md" . markdown-mode)
                                             ("pl6" . raku-mode)
                                             ("py" . python-mode)
                                             ("rb" . ruby-mode)
                                             ("rs" . rustic-mode)
                                             ("sqlite3" . sql-mode)
                                             ("ts" . typescript-mode)
                                             ("typescript" . typescript-mode)
                                             ("tsx" . web-mode)
                                             ("yaml". yaml-mode)
                                             ("zsh" . sh-mode)
                                             ("php" . php-mode))
                                           markdown-code-lang-modes))

  ;; markdown
  (add-hook 'markdown-mode #'orgtbl-mode))

(autoload-if-found '(mermaid-mode) "mermaid-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(autoload-if-found '(makefile-mode) "makefile-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))

(with-eval-after-load 'makefile-mode
  ;; config
  (setq makefile-electric-keys t)

  ;; hook
  (add-hook 'makefile-mode-hook #'flycheck-mode))

(autoload-if-found '(nasm-mode) "nasm-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.s$" . nasm-mode))

(autoload-if-found '(neon-mode) "neon-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.neon$" . neon-mode))

(autoload-if-found '(nim-mode) "nim-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(autoload-if-found '(ninja-mode) "ninja-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.ninja$" . ninja-mode))

(autoload-if-found '(nix-mode) "nix-mode" nil t)
(autoload-if-found '(nix-drv-mode) "nix-drv-mode" nil t)
(autoload-if-found '(nix-shell-unpack nix-shell-configure nix-shell-build) "nix-shell" nil t)
(autoload-if-found '(nix-repl) "nix-repl" nil t)
(autoload-if-found '(nix-format-before-save) "nix-format" nil t)

(add-to-list 'auto-mode-alist '("\\.nix$" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.drv$" . nix-drv-mode))

(add-hook 'before-save-hook #'nix-format-before-save)

(autoload-if-found '(nginx-mode) "nginx-mode" nil t)

(add-to-list 'auto-mode-alist '("nginx\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/.+\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(with-eval-after-load 'nginx-mode
  (setq nginx-indent-tabs-mode t))

(autoload-if-found '(nov-mode) "nov" nil t)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(with-eval-after-load 'nov
  (add-hook 'nov-mode-hook #'(lambda () (view-mode -1))))

(autoload-if-found '(pcap-mode) "pcap" nil t)

(add-to-list 'auto-mode-alist '("\\.pcap$" . pcap-mode))

(define-derived-mode phel-mode clojure-mode "Phel"
  "Major mode for editing Phel language source files."
  (setq-local comment-start "#")
  ;; We disable lockfiles so that ILT evaluation works.
  ;; The lockfiles seem to modify the buffer-file-name somehow, when the buffer changes
  ;; And that is detected by the currently running Phel process.
  ;; That interferes with evaluation, as the running Phel process starts behaving badly because of that.
  (setq-local create-lockfiles nil)
  )

(add-to-list 'auto-mode-alist '("\\.phel$" . phel-mode))

(autoload-if-found '(php-mode php-current-class php-current-namespace) "php-mode" nil t)
(autoload-if-found '(php-format-this-buffer-file
                     php-format-project
                     php-format-on-after-save-hook
                     php-format-auto-mode) "php-format" nil t)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(with-eval-after-load 'php-mode
  ;; hook
  (add-hook 'php-mode-hook #'php-format-auto-mode)

  ;; keybind
  (define-key php-mode-map (kbd "C-c C--") #'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") #'php-current-namespace)
  (define-key php-mode-map (kbd "C-.") nil)

  ;; config
  (setq php-mode-coding-style 'psr2)

  ;; phpstan
  (define-derived-mode phpstan-mode php-mode "phpstan"))

(autoload-if-found '(phpt-mode) "phpt-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.phpt$" . phpt-mode))

(autoload-if-found '(plantuml-mode) "plantuml-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.pu$" . plantuml-mode))

(autoload-if-found '(protobuf-mode) "protobuf-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(autoload-if-found '(pug-mode) "pug-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.pug$" . pug-mode))

(autoload-if-found '(prisma-mode) "prisma-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.prisma" . prisma-mode))

(autoload-if-found '(processing-mode) "processing-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(with-eval-after-load 'processing-mode
  (setq processing-location "/opt/processing/processing-java")
  (setq processing-output-dir "/tmp"))

(autoload-if-found '(python-mode) "python-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(autoload-if-found '(qt-pro-mode) "qt-pro-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

(autoload-if-found '(robots-txt-mode) "robots-txt-mode" nil t)

(add-to-list 'auto-mode-alist '("/robots\\.txt\\'" . robots-txt-mode))

(autoload-if-found '(ruby-mode) "ruby-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Schemafile" . ruby-mode))
(add-to-list 'auto-mode-alist '(".pryrc" . ruby-mode))
(add-to-list 'auto-mode-alist '("Fastfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Matchfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile" . ruby-mode))
(add-to-list 'auto-mode-alist '(".git-pr-template" . ruby-mode))
(add-to-list 'auto-mode-alist '(".gemrc" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.Brewfile" . ruby-mode))

(with-eval-after-load 'ruby-mode
  ;; config
  (setq ruby-insert-encoding-magic-comment nil))

(autoload-if-found '(rust-mode) "rust-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t))

(autoload-if-found '(scala-mode) "scala-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-to-list 'auto-mode-alist '("\\.scheme$" . scheme-mode))
(add-to-list 'auto-mode-alist '(".guix-channel" . scheme-mode))

(with-eval-after-load 'scheme
  (setq scheme-program-name "gosh -i"))

(autoload-if-found '(scad-mode) "scad-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode))

(autoload-if-found '(scss-mode) "scss-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . scss-mode))

(with-eval-after-load 'scss-mode
  (add-hook 'scss-mode-hook #'flycheck-mode))

(autoload-if-found '(shell-mode) "shell-mode" nil t)

(define-derived-mode console-mode shell-mode "console")

(autoload-if-found '(slim-mode) "slim-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))

(autoload-if-found '(solidity-mode) "solidity-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.sol$" . solidity-mode))

(autoload-if-found '(ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode) "ssh-config-mode" nil t)

(add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-config-mode))

(with-eval-after-load 'sql
  (load-library "sql-indent")

  ;; config
  (setq indent-tabs-mode nil)
  (setq sql-user "root")
  (setq sql-password "P@ssw0rd")
  (setq sql-server "127.0.0.1")
  (setq sql-port 13306)
  (setq sql-mysql-login-params '(server port user password database))

  ;; hook
  (add-hook 'sql-mode-hook #'flycheck-mode))

(autoload-if-found '(swift-mode) "swift-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode))

(autoload-if-found '(syslog-mode) "syslog-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.log$" . syslog-mode))

(autoload-if-found '(systemd-mode) "systemd" nil t)

(add-to-list 'auto-mode-alist '("\\.nspawn\\'" . systemd-mode))
(add-to-list 'auto-mode-alist `(,(rx (+? (any "a-zA-Z0-9-_.@\\")) "."
                                     (or "automount" "busname" "mount" "service" "slice"
                                         "socket" "swap" "target" "timer" "link" "netdev" "network")
                                     string-end)
                                . systemd-mode))
(add-to-list 'auto-mode-alist `(,(rx ".#"
                                     (or (and (+? (any "a-zA-Z0-9-_.@\\")) "."
                                              (or "automount" "busname" "mount" "service" "slice"
                                                  "socket" "swap" "target" "timer" "link" "netdev" "network"))
                                         "override.conf")
                                     (= 16 (char hex-digit)) string-end)
                                . systemd-mode))
(add-to-list 'auto-mode-alist `(,(rx "/systemd/" (+? anything) ".d/" (+? (not (any ?/))) ".conf" string-end)
                                . systemd-mode))

(autoload-if-found '(terraform-mode terraform-format-on-save-mode) "terraform-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))

(with-eval-after-load 'terraform-mode
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-hook 'terraform-mode-hook #'flycheck-mode))

(autoload-if-found '(conf-space-mode) "conf-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tigrc$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.editrc$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.inputrc$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.colorrc$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.asdfrc$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("credentials$" . conf-space-mode))

(autoload-if-found '(toml-mode) "toml-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))

(with-eval-after-load 'toml-mode
  (add-hook 'toml-mode-hook #'flycheck-mode))

(autoload-if-found '(tmux-mode) "tmux-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.tmux\\.conf$" . tmux-mode))

(autoload-if-found '(typescript-mode) "typescript-mode" nil t)

;; for ts/deno
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;; for tsx
(define-derived-mode typescript-tsx-mode typescript-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.jsx$" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-tsx-mode))

(autoload-if-found '(v-mode v-menu v-format-buffer) "v-mode" nil t)

(add-to-list 'auto-mode-alist '("\\(\\.v?v\\|\\.vsh\\)$" . v-mode))

(with-eval-after-load 'v-mode
  (define-key v-mode-map (kbd "M-z") #'v-menu)
  (define-key v-mode-map (kbd "C-c C-f") #'v-format-buffer))

(autoload-if-found '(vue-mode) "vue-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))

(with-eval-after-load 'vue-html-mode
  (setq vue-html-extra-indent 4))

(autoload-if-found '(vimrc-mode) "vimrc-mode" nil t)

(add-to-list 'auto-mode-alist '("vimrc" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(autoload-if-found '(wat-mode) "wat-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.wat?\\'" . wat-mode))

(autoload-if-found '(web-mode) "web-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svg$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid$" . web-mode))

(with-eval-after-load 'web-mode
  (setq web-mode-comment-style 2)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-indentation nil))

(autoload-if-found '(web-php-blade-mode) "web-php-blade-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.blade\\.php$" . web-php-blade-mode))

(autoload-if-found '(wolfram-mode run-wolfram) "wolfram-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
(add-to-list 'auto-mode-alist '("\\.nb$" . wolfram-mode))
(add-to-list 'auto-mode-alist '("\\.cbf$" . wolfram-mode))

(with-eval-after-load 'wolfram-mode
  (setq wolfram-path "path-to-dir"))

(autoload-if-found '(yaml-mode) "yaml-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.aclpolicy$" . yaml-mode))

(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook #'flycheck-mode))

(autoload-if-found '(yarn-mode) "yarn-mode" nil t)

(add-to-list 'auto-mode-alist '("yarn\\.lock\\'" . yarn-mode))

(autoload-if-found '(zig-mode) "zig-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.zig$" . zig-mode))

(autoload-if-found '(auto-save-buffers-enhanced) "auto-save-buffers-enhanced" nil t)

;; (add-hook 'window-setup-hook #'auto-save-buffers-enhanced)

(with-eval-after-load 'auto-save-buffers-enhanced
  (setq auto-save-buffers-enhanced-interval 10))

(autoload-if-found '(editorconfig-mode) "editorconfig" nil t)

(add-hook 'window-setup-hook #'editorconfig-mode)

(autoload-if-found '(persistent-scratch-setup-default) "persistent-scratch" nil t)

(add-hook 'window-setup-hook #'persistent-scratch-setup-default)

(with-eval-after-load 'persistent-scratch
  (setq persistent-scratch-autosave-interval 100))

(autoload-if-found '(popwin-mode) "popwin" nil t)

(add-hook 'window-setup-hook #'popwin-mode)

(autoload-if-found '(global-whitespace-mode) "whitespace" nil t)

(when window-system
  (add-hook 'window-setup-hook #'global-whitespace-mode))

(with-eval-after-load 'whitespace
  (setq whitespace-style '(face tabs tab-mark spaces space-mark))
  (setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])
                                        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t]))))

(autoload-if-found '(flycheck-mode flycheck-define-checker) "flycheck" nil t)

;; (add-hook 'window-setup-hook
;;           #'(lambda ()
;;               (flycheck-define-checker textlint
;;                 "A linter for prose."
;;                 :command ("npx" "textlint" "--format" "unix" source-inplace)
;;                 :error-patterns
;;                 ((warning line-start (file-name) ":" line ":" column ": "
;;                           (id (one-or-more (not (any " "))))
;;                           (message (one-or-more not-newline)
;;                                    (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;                           line-end))
;;                 :modes (org-mode))

;;               (with-eval-after-load 'flycheck
;;                 (add-to-list 'flycheck-checkers 'textlint))))

(autoload-if-found '(flycheck-elsa-setup) "flycheck-elsa" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup))

;; (eval-when-compile
;;   (el-clone :repo "nbfalcon/flycheck-projectile"))

;; (with-delayed-execution
;;   (message "Install flycheck-projectile...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/flycheck-projectile"))
;;   (autoload-if-found '(flycheck-projectile-list-errors) "flycheck-projectile" nil t))

(autoload-if-found '(md4rd
                     md4rd-login
                     md4rd-visit
                     md4rd-widget-expand-all
                     md4rd-widget-collapse-all
                     md4rd-reply
                     md4rd-upvote
                     md4rd-downvote
                     md4rd-widget-toggle-line
                     md4rd-refresh-login
                     md4rd-indent-all-the-lines) "md4rd" nil t)

(with-eval-after-load 'md4rd
  (add-hook 'md4rd-mode-hook #'md4rd-indent-all-the-lines)
  (run-with-timer 0 3540 #'md4rd-refresh-login)

  ;; config
  (setq md4rd-subs-active '(emacs lisp+Common_Lisp prolog clojure))
  ;; (setq md4rd--oauth-access-token "your-access-token-here")
  ;; (setq md4rd--oauth-refresh-token "your-refresh-token-here")

  ;; keymap
  (define-key md4rd-mode-map (kbd "u") 'tree-mode-goto-parent)
  (define-key md4rd-mode-map (kbd "o") 'md4rd-open)
  (define-key md4rd-mode-map (kbd "v") 'md4rd-visit)
  (define-key md4rd-mode-map (kbd "e") 'tree-mode-toggle-expand)
  (define-key md4rd-mode-map (kbd "E") 'md4rd-widget-expand-all)
  (define-key md4rd-mode-map (kbd "C") 'md4rd-widget-collapse-all)
  (define-key md4rd-mode-map (kbd "n") 'widget-forward)
  (define-key md4rd-mode-map (kbd "j") 'widget-forward)
  (define-key md4rd-mode-map (kbd "h") 'backward-button)
  (define-key md4rd-mode-map (kbd "p") 'widget-backward)
  (define-key md4rd-mode-map (kbd "k") 'widget-backward)
  (define-key md4rd-mode-map (kbd "l") 'forward-button)
  (define-key md4rd-mode-map (kbd "q") 'kill-current-buffer)
  (define-key md4rd-mode-map (kbd "r") 'md4rd-reply)
  (define-key md4rd-mode-map (kbd "u") 'md4rd-upvote)
  (define-key md4rd-mode-map (kbd "d") 'md4rd-downvote)
  (define-key md4rd-mode-map (kbd "t") 'md4rd-widget-toggle-line))

(autoload-if-found '(ansi-color-for-comint-mode-on) "ansi-color" nil t)

(with-eval-after-load 'shell-mode
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook
            #'(lambda ()
                (ansi-color-apply-on-region (point-min) (point-max)))))

(autoload-if-found '(highlight-indent-guides-mode) "highlight-indent-guides" nil t)

(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-method 'bitmap))

(autoload-if-found '(global-hl-todo-mode) "hl-todo" nil t)

(add-hook 'window-setup-hook #'global-hl-todo-mode)

(with-eval-after-load 'hl-todo
  (setq hl-todo-keyword-faces
          '(("HOLD" . "#d0bf8f")
            ("TODO" . "#cc9393")
            ("NOW" . "#dca3a3")
            ("SOMEDAY" . "#dc8cc3")
            ("WAIT" . "#7cb8bb")
            ("DONE" . "#afd8af")
            ("FIXME" . "#cc9393"))))

(autoload-if-found '(xterm-color-filter) "xterm-color" nil t)

(add-hook 'window-setup-hook
          #'(lambda () (setenv "TERM" "xterm-256color")))

(with-eval-after-load 'xterm-color
  (setq xterm-color-preserve-properties t))

(autoload-if-found '(amx-mode) "amx" nil t)

(add-hook 'window-setup-hook #'amx-mode)

(with-eval-after-load 'amx
  (setq amx-history-length 100))

(autoload-if-found '(global-corfu-mode) "corfu" nil t)

(add-hook 'window-setup-hook #'global-corfu-mode)

(with-eval-after-load 'corfu
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-cycle t)
  (setq corfu-on-exact-match nil))

(with-eval-after-load 'indent
  (setq tab-always-indent 'complete))

(autoload-if-found '(cape-file
                     cape-dabbrev
                     cape-elisp-block
                     cape-history
                     cape-keyword) "cape" nil t)

(with-eval-after-load 'minibuffer
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history))

(autoload-if-found '(prescient-persist-mode) "prescient" nil t)

(add-hook 'window-setup-hook #'prescient-persist-mode)

(with-eval-after-load 'prescient
  (setq prescient-aggressive-file-save t))

(autoload-if-found '(kind-icon-margin-formatter) "kind-icon" nil t)

(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(autoload-if-found '(avy-goto-word-1) "avy" nil t)

(keymap-global-set "C-:" #'avy-goto-word-1)

(with-eval-after-load 'avy
  (setq avy-all-windows nil)
  (setq avy-background t))

(autoload-if-found '(avy-zap-up-to-char-dwim) "avy-zap" nil t)

(keymap-global-set "M-z" 'avy-zap-up-to-char-dwim)

(autoload-if-found '(er/expand-region) "expand-region" nil t)

(add-hook 'window-setup-hook #'transient-mark-mode)

(keymap-global-set "C-M-@" 'er/expand-region)

(autoload-if-found '(mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this) "multiple-cursors" nil t)

(keymap-global-set "C->" #'mc/mark-next-like-this)
(keymap-global-set "C-<" #'mc/mark-previous-like-this)
(keymap-global-set "C-c C-<" #'mc/mark-all-like-this)

(keymap-global-set "M-d" #'my/delete-forward-block)

(defun my/delete-forward-block ()
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun my/define-word ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'define-word-at-point)
    (call-interactively #'define-word)))

(with-eval-after-load 'define-word
  (setq define-word-displayfn-alist
          '((wordnik . takeokunn/define-word--display-in-buffer)
            (openthesaurus . takeokunn/define-word--display-in-buffer)
            (webster . takeokunn/define-word--display-in-buffer)
            (weblio . takeokunn/define-word--display-in-buffer))))

(with-eval-after-load 'dired
  ;; config
  (setq dired-dwim-target nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil)

  ;; hook
  (add-hook 'dired-mode-hook #'(lambda () (display-line-numbers-mode -1))))

(autoload-if-found '(dired-collapse-mode) "dired-collapse" nil t)

(with-eval-after-load 'dired
  (add-hook 'dired-mode #'dired-collapse-mode))

(autoload-if-found '(dired-filter-mode) "dired-filter" nil t)

(with-eval-after-load 'dired
  (add-hook 'dired-mode #'dired-filter-mode))

(autoload-if-found '(dired-narrow-mode) "dired-narrow" nil t)

;; (with-eval-after-load 'dired
;;   (add-hook 'dired-mode-hook #'dired-narrow-mode))

(autoload-if-found '(dired-open-file) "dired-open" nil t)

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map [remap dired-find-file] #'dired-open-file))

(autoload-if-found '() "dired-ranger" nil t)

(autoload-if-found '(dired-quick-sort-setup) "dired-quick-sort" nil t)

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'dired-quick-sort-setup))

(autoload-if-found '(dired-subtree-apply-filter) "dired-subtree" nil t)

(autoload-if-found '(diredfl-global-mode) "diredfl" nil t)

(add-hook 'dired-mode-hook #'diredfl-global-mode)

(defun my/eww-rename-buffer ()
  "Rename the name of current EWW buffer."
  (let* ((title (plist-get eww-data :title))
         (url (file-name-base (eww-current-url)))
         (buffer-name (or (if (and title (> (length title) 0))
                              title
                            nil)
                          url "")))
    (rename-buffer (format "eww: %s" buffer-name) t)))

;; config
(with-eval-after-load 'eww
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "http://www.google.co.jp/search?q="))

;; keybind
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "C") #'eww-set-character-encoding)
  (define-key eww-mode-map (kbd "C-j") #'eww-follow-link)
  (define-key eww-mode-map (kbd "T") #'eww-goto-title-heading)
  (define-key eww-mode-map (kbd "T") #'eww-goto-title-heading))

;; hooks
(with-eval-after-load 'eww
  (add-hook 'eww-after-render #'my/eww-rename-buffer))

(autoload-if-found '(eww-lnum-follow eww-lnum-universal) "eww-lnum" nil t)

(with-eval-after-load 'eww
  (define-key eww-mode-map "f" #'eww-lnum-follow)
  (define-key eww-mode-map "F" #'eww-lnum-universal))

(autoload-if-found '(recentf-mode) "recentf" nil t)

(add-hook 'window-setup-hook #'recentf-mode)

(with-eval-after-load 'recentf
  (setq recentf-max-menu-items 10000)
  (setq recentf-max-saved-items 10000)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-save-file  "~/.emacs.d/.recentf")
  (setq recentf-exclude '(".recentf" "\\.gpg\\")))

(autoload-if-found '(open-junk-file) "open-junk-file" nil t)

(keymap-global-set "C-x j" #'open-junk-file)

(with-eval-after-load 'open-junk-file
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y-%m-%d-%H%M%S."))

(autoload-if-found '(vlf-disable-for-function) "vlf-setup" t)

(vlf-disable-for-function tags-verify-table "etags")
(vlf-disable-for-function tag-find-file-of-tag-noselect "etags")
(vlf-disable-for-function helm-etags-create-buffer "helm-tags")

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") #'dired-vlf))

(autoload-if-found '(font-lock-studio) "font-lock-studio" nil t)

(autoload-if-found '(gcmh-mode) "gcmh" nil t)

(add-hook 'window-setup-hook #'gcmh-mode)

(with-eval-after-load 'gcmh
  ;; config
  (setq gcmh-verbose t)

  ;; hooks
  (defvar my/gcmh-status nil)
  (advice-add #'garbage-collect
              :before
              (defun my/gcmh-log-start (&rest _)
                (when gcmh-verbose
                  (setq my/gcmh-status "Running GC..."))))

  (advice-add #'gcmh-message
              :override
              (defun my/gcmh-message (format-string &rest args)
                (setq my/gcmh-status
                      (apply #'format-message format-string args))
                (run-with-timer 2 nil
                                (lambda ()
                                  (setq my/gcmh-status nil))))))

(autoload-if-found '(global-git-commit-mode) "git-commit" nil t)
(autoload-if-found '(magit-status magit-blame) "magit")

(add-hook 'window-setup-hook #'global-git-commit-mode)

(defun my/magit-status ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (magit-status)))

(keymap-global-set "C-x g" #'my/magit-status)
(keymap-global-set "C-x G" #'magit-blame)

(with-eval-after-load 'magit
  (setq magit-refresh-status-buffer nil))

(with-eval-after-load 'magit-status
  ;; config
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          ;; magit-insert-merge-log
          ;; magit-insert-rebase-sequence
          ;; magit-insert-am-sequence
          ;; magit-insert-sequencer-sequence
          ;; magit-insert-bisect-output
          ;; magit-insert-bisect-rest
          ;; magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          ;; magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream-or-recent
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream))


  ;; keybind
  (define-key magit-status-mode-map (kbd "C-j") #'magit-visit-thing))

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "C-j") #'magit-visit-thing))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-h") #'delete-backward-char))

(autoload-if-found '(magit-file-icons-mode) "magit-file-icons" nil t)

;; (with-eval-after-load 'magit
;;   (add-hook 'magit-mode-hook #'magit-file-icons-mode))

(autoload-if-found '(magit-gptcommit-status-buffer-setup
                     magit-gptcommit-mode
                     magit-gptcommit-commit-accept) "magit-gptcommit" nil t)

;; (magit-gptcommit-status-buffer-setup)

(with-eval-after-load 'magit
  (add-hook 'magit-mode #'magit-gptcommit-mode))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-c C-g") #'magit-gptcommit-commit-accept))

;; (add-hook 'magit-mode-hook #'(lambda () (require 'forge)))

(autoload-if-found '(git-gutter-mode) "git-gutter" nil t)

(with-eval-after-load 'git-gutter
  ;; (add-hook 'prog-mode-hook #'git-gutter-mode)
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook)))

(autoload-if-found '(git-gutter-fr:init
                     git-gutter-fr:view-diff-infos
                     git-gutter-fr:clear) "git-gutter-fringe" nil t)

(with-eval-after-load 'git-gutter
  (setq git-gutter-fr:side 'right-fringe)
  (setq git-gutter:window-width -1)
  (setq git-gutter:init-function #'git-gutter-fr:init)
  (setq git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos)
  (setq git-gutter:clear-function #'git-gutter-fr:clear))

(autoload-if-found '(git-timemachine) "git-timemachine" nil t)

(autoload-if-found '(gist-mode) "gist" nil t)

(autoload-if-found '(blamer-mode) "blamer" nil t)

(autoload-if-found '(git-auto-commit-mode) "git-auto-commit-mode" nil t)

(with-eval-after-load 'git-auto-commit-mode
  (setq gac-automatically-push-p t)
  (setq gac-silent-message-p t)
  (setq gac-debounce-interval (* 60 60 3))
  (setq gac-default-message "Update"))

(autoload-if-found '(google-this) "google-this" nil t)

(autoload-if-found '(google-translate-at-point) "google-translate" nil t)

(autoload-if-found '(epa-file-enable) "epa-file" nil t)

(add-hook 'window-setup-hook #'epa-file-enable)

(with-eval-after-load 'epa-file
  (setq epa-file-encrypt-to '("bararararatty@gmail.com"))
  (setq epa-file-select-keys 'silent)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epg-pinentry-mode 'loopback)

  (fset 'epg-wait-for-status 'ignore))

(autoload-if-found '(pinentry-start) "pinentry" nil t)

(autoload-if-found '(helpful-callable
                     helpful-function
                     helpful-macro
                     helpful-command
                     helpful-key
                     helpful-variable
                     helpful-at-point) "helpful" nil t)
;; keybinds
(keymap-global-set "C-h f" #'helpful-callable)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h k" #'helpful-key)
(keymap-global-set "C-c C-d" #'helpful-at-point)
(keymap-global-set "C-h F" #'helpful-function)
(keymap-global-set "C-h C" #'helpful-command)

(autoload-if-found '(skk-mode) "ddskk-autoloads" nil t)

(keymap-global-set "C-x C-j" #'skk-mode)

(defun my/skk-C-j-key (arg)
  (interactive "P")
  (cond
   ((and (null (skk-in-minibuffer-p))
         (null skk-henkan-mode))
    (skk-emulate-original-map arg))
   (t
    (skk-kakutei arg))))

(with-eval-after-load 'skk
  ;; config
  (setq skk-preload t)
  (setq default-input-method "japanese-skk"))

(with-eval-after-load 'skk-vars
  ;; use skkserv
  (when-darwin
   (setq skk-server-host "localhost")
   (setq skk-server-portnum 1178))

  ;; guix
  (setq skk-byte-compile-init-file t)
  (setq skk-isearch-mode-enable 'always)
  (setq skk-egg-like-newline t)
  (setq skk-show-annotation nil)
  (setq skk-auto-insert-paren t)

  ;; azik
  (setq skk-use-azik t)
  (setq skk-azik-keyboard-type 'jp106)

  ;; ref: https://github.com/skk-dev/ddskk/blob/master/etc/dot.skk#L752-L768
  (add-to-list 'skk-rom-kana-rule-list '(skk-kakutei-key nil my/skk-C-j-key)))

(autoload-if-found '(ddskk-posframe-mode) "ddskk-posframe" nil t)

(with-eval-after-load 'skk
  (add-hook 'skk-mode-hook #'ddskk-posframe-mode))

(autoload-if-found '(hacker-typer) "hacker-typer" nil t)

(autoload-if-found '(power-mode) "power-mode" nil t)

(autoload-if-found '(sudden-death) "sudden-death" nil t)

(autoload-if-found '(redacted-mode) "redacted" nil t)

(defun my/redacted-mode ()
  (interactive)
  (read-only-mode (if redacted-mode -1 1))
  (redacted-mode (if redacted-mode -1 1)))

(autoload-if-found '(lorem-ipsum-insert-sentences
                     lorem-ipsum-insert-paragraphs
                     lorem-ipsum-insert-list) "lorem-ipsum" nil t)

(keymap-global-set "C-c C-l s" #'lorem-ipsum-insert-sentences)
(keymap-global-set "C-c C-l p" #'lorem-ipsum-insert-paragraphs)
(keymap-global-set "C-c C-l l" #'lorem-ipsum-insert-list)

(autoload-if-found '(key-chord-mode key-chord-define-global) "key-chord" nil t)

(add-hook 'window-setup-hook #'key-chord-mode)

;; for global
;; (key-chord-define-global "fj" #'view-mode)
;; (key-chord-define-global "jf" #'view-mode)

(autoload-if-found '(key-combo-mode key-combo-define-local) "key-combo" nil t)

;; for php-mode
(with-eval-after-load 'php-mode
  ;; (add-hook 'php-mode-hook
  ;;           #'(lambda ()
  ;;               (key-combo-mode)
  ;;               (when (window-system)
  ;;                 (key-combo-define-local (kbd ",>") " => "))
  ;;               ;; (key-combo-define-local (kbd "+") '("+" " + " "++" " ++ "))
  ;;               ;; (key-combo-define-local (kbd "-") '("-" " - " "--" " -- "))
  ;;               ;; (key-combo-define-local (kbd "*") '("*" "**" " * "))
  ;;               ;; (key-combo-define-local (kbd "=") '("=" " = " "==" "==="))
  ;;               ))
  )

;; for typescript-tsx-mode
(with-eval-after-load 'typescript-tsx-mode
  (add-hook 'typescript-tsx-mode
            #'(lambda ()
                (key-combo-mode)
                (key-combo-define-local (kbd "</") #'web-mode-element-close))))

(autoload-if-found '(which-key-mode) "which-key" nil t)

(add-hook 'window-setup-hook #'which-key-mode)

(autoload-if-found '(global-dmacro-mode) "dmacro" nil t)

(add-hook 'window-setup-hook #'global-dmacro-mode)

(autoload-if-found '(god-mode) "god-mode" nil t)

(autoload-if-found '(eglot) "eglot" nil t)

(with-eval-after-load 'eglot
  ;; config
  (setq eglot-events-buffer-size nil)
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)

  ;; language server
  ;; (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(clojure-mode clojurescript-mode clojurec-mode
  ;;                                                    . ("/Users/take/.emacs.d/.cache/lsp/clojure/clojure-lsp"
  ;;                                                       "listen" "--verbose")))
  )

(autoload-if-found '(lsp lsp-deferred lsp-org lsp-register-client make-lsp-client) "lsp-mode" nil t)
(autoload-if-found '(lsp-lens-mode lsp-lens-refresh lsp-lens--enable) "lsp-lens" nil t)
(autoload-if-found '(lsp-modeline-workspace-status-mode) "lsp-modeline" nil t)
(autoload-if-found '(lsp-headerline-breadcrumb-mode) "lsp-headerline" nil t)
(autoload-if-found '(lsp-diagnostics-mode) "lsp-diagnostics" nil t)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '("php-ts-mode" . "php"))

  ;; ignore path
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]storage")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docs")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].calva")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].clj-kondo")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].direnv")

  ;; enable flycheck
  (add-hook 'lsp-mode-hook #'flycheck-mode)

  ;; enable diagnostics
  (add-hook 'lsp-configure-hook #'lsp-diagnostics-mode)

  ;; config
  (setq lsp-idle-delay 0.8)
  (setq lsp-enable-links nil)
  (setq lsp-log-io nil)
  (setq lsp-file-watch-threshold 20000))

(with-eval-after-load 'lsp-rename
  (advice-add 'lsp-rename :before #'(lambda (&rest _) (remove-hook 'find-file-hooks #'view-mode)))
  (advice-add 'lsp-rename :after #'(lambda (&rest _) (add-hook 'find-file-hooks #'view-mode))))

(with-eval-after-load 'lsp-diagnostics
  (setq lsp-diagnostics-flycheck-default-level 'info))

(with-eval-after-load 'lsp-completion
  (setq lsp-completion-no-cache t)
  (setq lsp-prefer-capf t))

(with-eval-after-load 'lsp-php
  ;; for intelephense
  (setq lsp-intelephense-telemetry-enabled t)
  (setq lsp-intelephense-files-exclude ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
                                          "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"
                                          "**/.direnv/**"]))

(with-eval-after-load 'lsp-javascript
  ;; for typescript-language-server
  (setq lsp-clients-typescript-log-verbosity "info")
  (setq lsp-typescript-references-code-lens-enabled t)
  (setq lsp-typescript-implementations-code-lens-enabled t)
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-javascript-display-parameter-type-hints t)
  (setq lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (setq lsp-javascript-display-property-declaration-type-hints t)
  (setq lsp-javascript-display-variable-type-hints t))

(with-eval-after-load 'lsp-completion
  (setq lsp-completion-provider :none))

(with-eval-after-load 'lsp-ruby
  (setq lsp-solargraph-autoformat t)
  (setq lsp-solargraph-multi-root nil))

(with-eval-after-load 'lsp-nil
  (setq lsp-nix-nil-max-mem 100000))

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'(lambda () (require 'ccls))))

(with-eval-after-load 'lsp-php
  (setq lsp-intelephense-licence-key "00OXTX8OROOJH9P"))

(autoload-if-found '(consult-lsp-symbols) "consult-lsp" nil t)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(autoload-if-found '(lsp-treemacs-sync-mode) "lsp-treemacs" nil t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-treemacs-sync-mode))

(with-eval-after-load 'lsp-treemacs
  (setq lsp-treemacs-error-list-severity 1)
  (setq lsp-treemacs-error-list-current-project-only t))

(autoload-if-found '(lsp-docker-start) "lsp-docker" nil t)

(autoload-if-found '(dap-debug) "dap-mode" nil t)
(autoload-if-found '(dap-hydra) "dap-hydra" nil t)
(autoload-if-found '(dap-ui-mode dap-ui-controls-mode) "dap-ui" nil t)
(autoload-if-found '(dap-tooltip-mode) "dap-mouse" nil t)
(autoload-if-found '(dap-php-setup) "dap-php" nil t)
(autoload-if-found '(dap-node-setup) "dap-node" nil t)
(autoload-if-found '(dap-go-setup) "dap-go" nil t)
(autoload-if-found '(dap-ruby-setup) "dap-ruby" nil t)

(with-eval-after-load 'dap-mode
  ;; keybind
  (define-key dap-mode-map (kbd "C-c d") #'dap-breakpoint-toggle)

  ;; hook
  (add-hook 'dap-mode-hook #'dap-ui-mode)
  (add-hook 'dap-mode-hook #'dap-ui-controls-mode)
  (add-hook 'dap-mode-hook #'tooltip-mode)
  (add-hook 'dap-mode-hook #'dap-tooltip-mode)
  (add-hook 'dap-stopped-hook #'(lambda (arg) (call-interactively #'dap-hydra))))

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook #'dap-php-setup))

(with-eval-after-load 'dap-php
  ;; config
  (setq dap-php-debug-path `,(expand-file-name "xdebug/vscode-php-debug" dap-utils-extension-path))

  ;; register
  (dap-register-debug-template "Laravel Run Configuration"
                               (list :type "php"
                                     :request "launch"
                                     :mode "remote"
                                     :host "localhost"
                                     :port 9003)))

;; (with-eval-after-load 'js2-mode
;;   (add-hook 'js2-mode-hook #'dap-node-setup))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'dap-go-setup))

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'dap-ruby-setup))

(autoload-if-found '(lsp-ui-mode) "lsp-ui" nil t)

;; hook
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

;; lsp-ui-doc
(with-eval-after-load 'lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-delay 1)
  (setq lsp-ui-doc-max-height 30))

;; lsp-ui-peek
(autoload-if-found '(lsp-ui-peek-find-references lsp-ui-peek-find-definitions lsp-ui-peek-find-implementation) "lsp-ui-peek" nil t)
(with-eval-after-load 'lsp-ui-peek
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-peek-peek-height 30)
  (setq lsp-ui-peek-list-width 60)
  (setq lsp-ui-peek-fontify 'on-demand))

;; lsp-ui-imenu
(autoload-if-found '(lsp-ui-imenu) "lsp-ui-imenu" nil t)
(with-eval-after-load 'lsp-ui-imenu
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-imenu-kind-position 'top))

;; lsp-ui-sideline
(autoload-if-found '(lsp-ui-sideline-mode) "lsp-ui-sideline" nil t)
(with-eval-after-load 'lsp-ui-sideline
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover t))

;; keybind
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-r") #'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "C-c C-j") #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map (kbd "C-c C-i") #'lsp-ui-peek-find-implementation)
  (define-key lsp-mode-map (kbd "C-c C-m") #'lsp-ui-imenu)
  (define-key lsp-mode-map (kbd "C-c C-s") #'lsp-ui-sideline-mode)
  (define-key lsp-mode-map (kbd "C-c C-d") #'lsp-ui-doc-mode))

(autoload-if-found '(lsp-scheme) "lsp-scheme" nil t)

(with-eval-after-load 'scheme
  ;; (add-hook 'scheme-mode-hook #'lsp-scheme)
  )

(with-eval-after-load 'lsp-scheme
  (setq lsp-scheme-implementation "guile"))

(autoload-if-found '(lsp) "lsp-haskell" nil t)

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

;; (eval-when-compile
;;   (el-clone :repo "emacs-lsp/lsp-pyright"))

;; (with-delayed-execution
;;   (message "Install lsp-pyright...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-pyright"))

;;   ;; (with-eval-after-load 'python-mode
;;   ;;   (add-hook 'python-mode-hook #'(lambda ()
;;   ;;                                   (require 'lsp-pyright)
;;   ;;                                   (lsp))))
;;   )

;; (eval-when-compile
;;   (el-clone :repo "manateelazycat/lsp-bridge"))

;; (with-delayed-execution
;;   (message "Install lsp-bridge...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-bridge"))

;;   (autoload-if-found '(lsp-bridge-mode) "lsp-bridge" nil t)

;;   (with-eval-after-load 'lsp-bridge
;;     ;; config
;;     (setq lsp-bridge-php-lsp-server "phpactor")

;;     ;; keybind
;;     (define-key lsp-bridge-mode-map (kbd "M-.") #'lsp-bridge-find-impl)
;;     (define-key lsp-bridge-mode-map (kbd "C-c C-r") #'lsp-bridge-find-references)))

(autoload-if-found '(mu4e) "mu4e" nil t)

(keymap-global-set "C-x C-w" #'mu4e)

(with-eval-after-load 'mu4e
  ;; config
  (setq mail-user-agent 'mu4e-user-agent))

(with-eval-after-load 'mu4e-update
  ;; config
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-index-cleanup nil)
  (setq mu4e-index-lazy-check t))

(with-eval-after-load 'mu4e-view
  ;; config
  (setq mu4e-split-view 'horizontal))

(with-eval-after-load 'mu4e-folders
  ;; config
  (setq mu4e-maildir-shortcuts '((:maildir "/INBOX" :key ?i)
                                   (:maildir "/Redmine" :key ?r)
                                   (:maildir "/GitHub" :key ?g)
                                   (:maildir "/Emacs" :key ?e)
                                   (:maildir "/Guix" :key ?u))))

(with-eval-after-load 'mm-decode
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; (eval-when-compile
;;   (el-clone :repo "lordpretzel/mu4e-views"))

;; (with-delayed-execution
;; (message "Install mu4e-views...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/mu4e-views"))

;; (autoload-if-found '() "mu4e-dashboard" nil t)

(autoload-if-found '(clm/toggle-command-log-buffer) "command-log-mode" nil t)

(defalias 'command-log #'clm/toggle-command-log-buffer)

(autoload-if-found '(fancy-narrow-mode) "fancy-narrow" nil t)

;; (with-eval-after-load 'org
;;   (add-hook 'org-mode-hook #'fancy-narrow-mode))

;; (with-eval-after-load 'elisp-mode
;;   (add-hook 'emacs-lisp-mode-hook #'fancy-narrow-mode))

;; (with-eval-after-load 'lisp-mode
;;   (add-hook 'lisp-mode-hook #'fancy-narrow-mode))

;; (with-eval-after-load 'clojure-mode
;;   (add-hook 'clojure-mode-hook #'fancy-narrow-mode))

(autoload-if-found '(global-origami-mode origami-recursively-toggle-node origami-recursively-toggle-node) "origami" nil t)

(add-hook 'window-setup-hook #'global-origami-mode)

(keymap-global-set "C-c t" #'origami-recursively-toggle-node)
(keymap-global-set "C-c C-t" #'origami-recursively-toggle-node)

(autoload-if-found '(proced) "proced" nil t)

(add-hook 'proced-mode-hook #'proced-toggle-auto-update)

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 10)
  (setq proced-tree-flag t)
  (setq proced-format 'long))

(autoload-if-found '(proced-narrow) "proced-narrow" nil t)

(with-eval-after-load 'proced
  (define-key proced-mode-map (kbd "/") #'proced-narrow))

(autoload-if-found '(projectile-mode) "projectile" nil t)

(add-hook 'window-setup-hook #'projectile-mode)

(with-eval-after-load 'projectile
  ;; keybind
  (keymap-global-set "M-p" #'projectile-command-map)
  (keymap-global-set "C-c p" #'projectile-command-map)

  ;; hook
  (add-hook 'projectile-mode-hook #'my/update-projectile-known-projects)

  ;; config
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)

  ;; function
  (defun my/update-projectile-known-projects ()
    (interactive)
    (projectile-clear-known-projects)
    (projectile-cleanup-known-projects)
    (setq projectile-known-projects (mapcar
                                       (lambda (x)
                                         (abbreviate-file-name (concat x "/")))
                                       (split-string (shell-command-to-string "ghq list --full-path"))))))

(autoload-if-found '(consult-projectile-switch-to-buffer
                     consult-projectile-switch-to-buffer-other-window
                     consult-projectile-switch-to-buffer-other-frame
                     consult-projectile-find-dir
                     consult-projectile-find-file
                     consult-projectile-find-file-other-window
                     consult-projectile-find-file-other-frame
                     consult-projectile-recentf
                     consult-projectile-switch-project) "consult-projectile" nil t)

(with-eval-after-load 'projectile
  (advice-add 'projectile-switch-to-buffer :override #'consult-projectile-switch-to-buffer)
  (advice-add 'projectile-switch-to-buffer-other-window :override #'consult-projectile-switch-to-buffer-other-window)
  (advice-add 'projectile-switch-to-buffer-other-frame :override #'consult-projectile-switch-to-buffer-other-frame)
  (advice-add 'projectile-find-dir :override #'consult-projectile-find-dir)
  (advice-add 'projectile-find-file :override #'consult-projectile-find-file)
  (advice-add 'projectile-find-file-other-window :override #'consult-projectile-find-file-other-window)
  (advice-add 'projectile-find-file-other-frame :override #'consult-projectile-find-file-other-frame)
  (advice-add 'projectile-recentf :override #'consult-projectile-recentf)
  (advice-add 'projectile-switch-project :override #'consult-projectile-switch-project))

(autoload-if-found '(emr-show-refactor-menu) "emr" nil t)

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "M-RET") #'emr-show-refactor-menu))

;; (autoload-if-found '(migemo-init) "migemo" nil t)

;; (add-hook 'window-setup-hook #'migemo-init)

;; (with-eval-after-load 'migemo
;;   (setq migemo-command "cmigemo")
;;   (setq migemo-dictionary "~/.nix-profile/share/migemo/utf-8/migemo-dict")
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-coding-system 'utf-8-unix)
;;   (setq migemo-use-pattern-alist t)
;;   (setq migemo-use-frequent-pattern-alist t))

(autoload-if-found '(wgrep-setup) "wgrep" nil t)

(with-eval-after-load 'grep
  (add-hook 'grep-setup-hook 'wgrep-setup))

(autoload-if-found '(consult-bookmark
                     consult-buffer
                     consult-buffer-other-frame
                     consult-buffer-other-tab
                     consult-buffer-other-window
                     consult-complex-command
                     consult-find
                     consult-flycheck
                     consult-focus-lines
                     consult-git-grep
                     consult-global-mark
                     consult-goto-line
                     consult-grep
                     consult-history
                     consult-isearch-history
                     consult-keep-lines
                     consult-line
                     consult-line-multi
                     consult-locate
                     consult-man
                     consult-mark
                     consult-outline
                     consult-project-buffer
                     consult-register
                     consult-register-load
                     consult-register-store
                     consult-ripgrep
                     consult-yank-pop
                     consult-mode-command

                     ;; other
                     consult-preview-at-point-mode
                     consult-register-window) "consult" nil t)
(autoload-if-found '(consult-compile-error) "consult-compile" nil t)
;; (autoload-if-found '(consult-org-heading consult-org-agenda) "consult-org" nil t)
(autoload-if-found '(consult-imenu consult-imenu-multi) "consult-imenu" nil t)
(autoload-if-found '(consult-kmacro) "consult-kmacro" nil t)
(autoload-if-found '(consult-xref) "consult-xref" nil t)

;; keybind
;; C-c bindings in `mode-specific-map'
(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "C-c h" #'consult-history)

;; C-x bindings in `ctl-x-map'
(keymap-global-set "C-x M-:" #'consult-complex-command)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)

;; Other custom bindings
(keymap-global-set "M-y" #'consult-yank-pop)

;; M-g bindings in `goto-map'
(keymap-global-set "M-g e" #'consult-compile-error)
(keymap-global-set "M-g f" #'consult-flycheck)
(keymap-global-set "M-g g" #'consult-goto-line)
(keymap-global-set "M-g M-g" #'consult-goto-line)
(keymap-global-set "M-g o" #'consult-outline)
(keymap-global-set "M-g m" #'consult-mark)
(keymap-global-set "M-g k" #'consult-global-mark)
(keymap-global-set "M-g i" #'consult-imenu)
(keymap-global-set "M-g I" #'consult-imenu-multi)

;; C-o bindings in `search-map'
(keymap-global-set "C-o" #'(lambda ()
                             (interactive)
                             (let ((word (thing-at-point 'symbol 'no-properties)))
                               (consult-line word))))

;; Isearch integration
(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history))

;; Minibuffer history
(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "M-s") #'consult-history)
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history))

(with-eval-after-load 'simple
  (add-hook 'completion-list-mode #'consult-preview-at-point-mode))

(with-eval-after-load 'register
  (advice-add #'register-preview :override #'consult-register-window))

(with-eval-after-load 'xref
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(autoload-if-found '(affe-grep) "affe" nil t)

(with-eval-after-load 'affe
  (setq affe-highlight-function 'orderless-highlight-matches)
  (setq affe-find-command "fd --color=never --full-path")
  (setq affe-regexp-function 'orderless-pattern-compiler))

(autoload-if-found '(compile-multi) "compile-multi" nil t)
(autoload-if-found '(consult-compile-multi-mode) "consult-compile-multi" nil t)
(autoload-if-found '(compile-multi-embark-mode) "compile-multi-embark" nil t)

(keymap-global-set "C-x m" #'compile-multi)

;; (with-eval-after-load 'consult
;;   (consult-compile-multi-mode))

;; (with-eval-after-load 'embark
;;   (compile-multi-embark-mode))

(autoload-if-found '(vertico-mode) "vertico" nil t)
(autoload-if-found '(vertico-directory-tidy
                     vertico-directory-enter
                     vertico-directory-delete-char
                     vertico-directory-delete-word) "vertico-directory" nil t)
(autoload-if-found '(vertico-flat-mode) "vertico-flat" nil t)

(add-hook 'window-setup-hook #'vertico-mode)

(with-eval-after-load 'rfn-eshadow
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(with-eval-after-load 'vertico
  (setq vertico-count 8)
  (setq vertico-cycle t))

(autoload-if-found '(marginalia-mode) "marginalia" nil t)

;; (add-hook 'window-setup-hook #'marginalia-mode)

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))

(autoload-if-found '(orderless-all-completions orderless-try-completion) "orderless" nil t)

(with-eval-after-load 'minibuffer
  ;; config
  (setq completion-styles '(orderless basic))
  ;; (setq completion-category-overrides '((file (styles basic partial-completion))))
  (add-to-list 'completion-styles-alist '(orderless orderless-try-completion orderless-all-completions
                                                    "Completion of multiple components, in any order.")))

(autoload-if-found '(exec-path-from-shell-initialize) "exec-path-from-shell")

(add-hook 'window-setup-hook #'exec-path-from-shell-initialize)

(with-eval-after-load 'exec-path-from-shell
  (setq exec-path-from-shell-variables '("PATH"
                                           "GEM_HOME"
                                           "GOROOT"
                                           "GOPATH"
                                           "LSP_USE_PLISTS"
                                           "TERM"
                                           "SSH_AUTH_SOCK"
                                           "NATIVE_FULL_AOT"
                                           "GPG_TTY")))

(autoload-if-found '(yas-global-mode) "yasnippet" nil t)

(add-hook 'window-setup-hook #'yas-global-mode)

(autoload-if-found '(consult-yasnippet) "consult-yasnippet" nil t)

(keymap-global-set "C-c y" #'consult-yasnippet)
(keymap-global-set "C-c C-y" #'consult-yasnippet)

(autoload-if-found '(esup) "esup" nil t)

(autoload-if-found '(explain-pause-mode) "explain-pause-mode" nil t)

(autoload-if-found '(disk-usage disk-usage-here) "disk-usage" nil t)

(autoload-if-found '(keyfreq-mode keyfreq-autosave-mode) "keyfreq" nil t)

(add-hook 'window-setup-hook #'keyfreq-mode)
(add-hook 'window-setup-hook #'keyfreq-autosave-mode)

(autoload-if-found '(uptimes) "uptimes" nil t)

(autoload-if-found '(global-syntax-subword-mode) "syntax-subword" nil t)

(add-hook 'window-setup-hook #'global-syntax-subword-mode)

(autoload-if-found '(symon-mode) "symon" nil t)

(autoload-if-found '(tab-bar-mode
                     tab-bar-history-mode
                     tab-previous
                     tab-next) "tab-bar" nil t)

(add-hook 'window-setup-hook #'tab-bar-history-mode)

(keymap-global-set "C-x C-t" tab-prefix-map)
(keymap-global-set "M-[" #'tab-previous)
(keymap-global-set "M-]" #'tab-next)

;; rename tab-bar with projectile
(define-key tab-prefix-map (kbd "r") #'my/tab-bar-rename-tab)

(with-eval-after-load 'tab-bar
  ;; config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'left)
  (setq tab-bar-history-mode nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-tab-name-truncated-max 12)
  (setq tab-bar-separator " | ")

  ;; advice
  ;; close neotree when tab bar action
  (advice-add 'tab-new :before #'(lambda (&rest _) (neotree-hide)))
  (advice-add 'tab-next :before #'(lambda (&rest _) (neotree-hide)))
  (advice-add 'tab-bar-switch-to-tab :before #'(lambda (&rest _) (neotree-hide)))

  ;; function
  (defun my/tab-bar-rename-tab ()
    (interactive)
    (let ((proj-name (projectile-project-name)))
      (tab-bar-rename-tab proj-name)))

  ;; hook
  (add-hook 'tab-bar-mode-hook #'(lambda () (display-line-numbers-mode -1))))

(autoload-if-found '(auto-insert-mode) "autoinsert" nil t)

(add-hook 'window-setup-hook #'auto-insert-mode)

(with-eval-after-load 'autoinsert
  (setq auto-insert-directory "~/.emacs.d/auto-insert"))

(autoload-if-found '(nerd-icons-dired-mode) "nerd-icons-dired" nil t)

(with-eval-after-load 'dired-mode
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(autoload-if-found '(nerd-icons-completion-marginalia-setup) "nerd-icons-completion" nil t)

;; (add-hook 'window-setup-hook #'nerd-icons-completion-marginalia-setup)

(autoload-if-found '(dashboard-refresh-buffer) "dashboard" nil t)

(with-eval-after-load 'dashboard
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 4)
  (setq dashboard-items '((recents . 10))))

(autoload-if-found '(dimmer-configure-which-key
                     dimmer-configure-org
                     dimmer-mode)
                   "dimmer" nil t)

(add-hook 'window-setup-hook #'dimmer-mode)
(add-hook 'window-setup-hook #'dimmer-configure-which-key)
;; (add-hook 'window-setup-hook #'dimmer-configure-org)

(autoload-if-found '(doom-themes-enable-org-fontification) "doom-themes-ext-org" nil t)

;; (add-hook 'emacs-startup-hook #'doom-themes-enable-org-fontification)
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (when (require 'doom-themes)
                (load-theme 'doom-dracula t))))

(with-eval-after-load 'doom-themes
  ;; config
  (setq doom-themes-padded-modeline t)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t))

(autoload-if-found '(doom-modeline-mode) "doom-modeline" nil t)

(add-hook 'emacs-startup-hook #'doom-modeline-mode)
(add-hook 'emacs-startup-hook #'(lambda () (line-number-mode 0)))
(add-hook 'emacs-startup-hook #'(lambda () (column-number-mode 0)))

(with-eval-after-load 'doom-modeline
  ;; config
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon `,(display-graphic-p))
  (setq doom-modeline-major-mode-icon `,(display-graphic-p))
  (setq doom-modeline-minor-modes nil))

(autoload-if-found '(global-hl-line-mode) "hl-line-mode" nil t)

(add-hook 'window-setup-hook
          #'(lambda ()
              (when (not window-system)
                (global-hl-line-mode))))

(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :inherit nil)
  (set-face-background 'hl-line "#444642"))

(autoload-if-found '(idle-highlight-mode) "idle-highlight-mode" nil t)

(with-eval-after-load 'idle-highlight-mode
  (setq idle-highlight-idle-time 0.1))

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'idle-highlight-mode))

(autoload-if-found '(neotree-hide neotree-dir neotree-make-executor neo-open-file neo-open-dir) "neotree" nil t)

(defun my/neotree-toggle ()
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory ".neotree")
                               (locate-dominating-file default-directory ".git"))))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-dir default-directory))))

(keymap-global-set "C-q" #'my/neotree-toggle)

(with-eval-after-load 'neotree
  ;; config
  (setq neo-theme 'ascii)
  (setq neo-show-hidden-files t)

  ;; hook
  (add-hook 'neotree-mode-hook #'(lambda () (display-line-numbers-mode -1)))

  ;; keybind
  (define-key neotree-mode-map (kbd "C-j") (neotree-make-executor
                                            :file-fn #'neo-open-file
                                            :dir-fn  #'neo-open-dir)))

(autoload-if-found '(nyan-mode) "nyan-mode" nil t)

(add-hook 'emacs-startup-hook #'nyan-mode)

(with-eval-after-load 'nyan-mode
  (setq nyan-cat-face-number 5)
  (setq nyan-animate-nyancat t))

(autoload-if-found '(volatile-highlights-mode) "volatile-highlights" nil t)

(add-hook 'emacs-startup-hook #'volatile-highlights-mode)

(autoload-if-found '(idle-highlight-mode) "idle-highlight-mode" nil t)

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'idle-highlight-mode))

(with-eval-after-load 'idle-highlight-mode
  (setq idle-highlight-idle-time 0.1))

(autoload-if-found '(global-undo-tree-mode) "undo-tree" nil t)

(add-hook 'window-setup-hook #'global-undo-tree-mode)

(with-eval-after-load 'files
  (add-hook 'find-file-hooks #'view-mode))

(with-eval-after-load 'view
  ;; hooks
  (add-hook 'view-mode-hook #'my/enable-view-mode-automatically)

  ;; keybind
  (define-key view-mode-map (kbd "f") #'forward-char)
  (define-key view-mode-map (kbd "b") #'backward-char)
  (define-key view-mode-map (kbd "n") #'my/org-view-next-heading)
  (define-key view-mode-map (kbd "p") #'my/org-view-previous-heading)
  (define-key view-mode-map (kbd "@") #'set-mark-command)
  (define-key view-mode-map (kbd "C-c '") #'my/org-edit-special)
  (define-key view-mode-map (kbd "C-c C-C") #'my/org-ctrl-c-ctrl-c)
  (define-key view-mode-map (kbd "e") nil)
  (define-key view-mode-map (kbd "C-j") nil)
  (define-key view-mode-map (kbd "C-i") #'my/view-tab)
  (define-key view-mode-map (kbd "S-C-i") #'my/view-shifttab)

  ;; functions
  (defun my/org-view-next-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-next-visible-heading 1)
      (next-line)))

  (defun my/org-view-previous-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
        (org-previous-visible-heading 1)
      (previous-line)))

  (defun my/view-tab ()
    (interactive)
    (when (and (derived-mode-p 'org-mode)
               (or (org-at-heading-p)
                   (org-at-property-drawer-p)))
      (let ((view-mode nil))
        (org-cycle))))

  (defun my/view-shifttab ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (let ((view-mode nil))
        (org-shifttab))))

  (defun my/org-edit-special ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (view-mode -1)
      (org-edit-special)))

  (defun my/org-ctrl-c-ctrl-c ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (view-mode -1)
      (org-ctrl-c-ctrl-c)))

  (defvar my/view-mode-timer nil)
  (defun my/enable-view-mode-automatically ()
    (if view-mode
        (when my/view-mode-timer
          (cancel-timer my/view-mode-timer))
      (setq my/view-mode-timer (run-with-idle-timer (* 60 10) nil #'view-mode))))

  ;; advices
  (advice-add 'view--disable :before #'(lambda (&rest _) (view-lock-mode -1))))

(autoload-if-found '(view-lock-timer-start view-lock-quit) "view-lock-mode" nil t)

(with-eval-after-load 'view
  (add-hook 'view-mode-hook #'view-lock-timer-start))

(with-eval-after-load 'view-lock-mode
  (setq view-lock-start-time (* 30 60)))

(autoload-if-found '(pass pass-view-mode) "pass" nil t)

(add-to-list 'auto-mode-alist (cons (substitute-in-file-name "$HOME/ghq/github.com/takeokunn/password-store/.*\\.gpg") 'pass-view-mode))

(with-eval-after-load 'pass
  (setq pass-suppress-confirmations t))

(with-eval-after-load 'comint
  (setq comint-buffer-maximum-size 100000)
  (setq comint-prompt-read-only t)
  (setq comint-terminfo-terminal "eterm-256color"))

(autoload-if-found '(crux-open-with
                     crux-smart-open-line-above
                     crux-cleanup-buffer-or-region
                     crux-view-url
                     crux-transpose-windows
                     crux-duplicate-current-line-or-region
                     crux-duplicate-and-comment-current-line-or-region
                     crux-rename-file-and-buffer
                     crux-visit-term-buffer
                     crux-kill-other-buffers
                     crux-indent-defun
                     crux-top-join-lines
                     crux-kill-line-backwards) "crux" nil t)

;; keybind
(keymap-global-set "C-c o" #'crux-open-with)
(keymap-global-set "C-S-o" #'crux-smart-open-line-above)
(keymap-global-set "C-c u" #'crux-view-url)
(keymap-global-set "C-x 4 t" #'crux-transpose-windows)
(keymap-global-set "C-c d" #'crux-duplicate-current-line-or-region)
(keymap-global-set "C-c M-d" #'crux-duplicate-and-comment-current-line-or-region)
(keymap-global-set "C-c r" #'crux-rename-file-and-buffer)
(keymap-global-set "C-c M-t" #'crux-visit-term-buffer)
(keymap-global-set "C-c k" #'crux-kill-other-buffers)
(keymap-global-set "C-M-z" #'crux-indent-defun)
(keymap-global-set "C-^" #'crux-top-join-lines)
(keymap-global-set "C-DEL" #'crux-kill-line-backwards)

(autoload-if-found '(delete-selection-mode) "delsel" nil t)

(add-hook 'window-setup-hook #'delete-selection-mode)

(autoload-if-found '(dogears-go
                     dogears-back
                     dogears-forward
                     dogears-list
                     dogears-sidebar) "dogears" nil t)

;; keybind
(keymap-global-set "M-g d" #'dogears-go)
(keymap-global-set "M-g M-b" #'dogears-back)
(keymap-global-set "M-g M-f" #'dogears-forward)
(keymap-global-set "M-g M-d" #'dogears-list)
(keymap-global-set "M-g M-D" #'dogears-sidebar)

(autoload-if-found '(embark-act embark-dwim embark-prefix-help-command) "embark" nil t)
(autoload-if-found '(embark-consult-outline-candidates
                     embark-consult-imenu-candidates
                     embark-consult-imenu-or-outline-candidates) "embark-consult" nil t)

(keymap-global-set "C-." #'embark-act)
(keymap-global-set "C-h B" #'embark-prefix-help-command)

(with-eval-after-load 'embark
  ;; macros
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn)))))

  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last (split-string
                                         (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn)))

  (defun my/sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|sudo:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/sudo:root@localhost:" file))))

  ;; config
  (setq embark-mixed-indicator-delay 0.1)
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; ace-window
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  ;; split window(2)
  (define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

  ;; split window(3)
  (define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))

  ;; sudo
  (define-key embark-file-map (kbd "S") #'my/sudo-find-file)

  ;; consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(autoload-if-found '(goto-address-prog-mode goto-address-mode) "goto-address" nil t)

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

(with-eval-after-load 'text-mode
  (add-hook 'text-mode-hook #'goto-address-mode))

(with-eval-after-load 'htmlize
  (setq htmlize-html-charset 'utf-8))

(autoload-if-found '(midnight-mode) "midnight" nil t)

(add-hook 'window-setup-hook #'midnight-mode)

(with-eval-after-load 'midnight
  (setq clean-buffer-list-delay-general 1))

(autoload-if-found '(minimap-mode) "minimap" nil t)

(keymap-global-set "C-c m" #'minimap-mode)

(with-eval-after-load 'minimap
  (setq minimap-window-location 'right)
  (setq minimap-update-delay 0.2)
  (setq minimap-minimum-width 20)
  (setq minimap-major-modes '(prog-mode org-mode)))

(autoload-if-found '(puni-global-mode puni-disable-puni-mode) "puni" nil t)

(add-hook 'window-setup-hook #'puni-global-mode)

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'lisp-interaction-mode
  (add-hook 'lisp-interacton-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'simple
  (add-hook 'eval-expression-minibuffer-setup-hook #'puni-disable-puni-mode))

(with-eval-after-load 'ielm
  (add-hook 'inferior-emacs-lisp-mode-hook #'puni-disable-puni-mode))

(with-eval-after-load 'minibuffer
  (add-hook 'minibuffer-mode-hook #'puni-disable-puni-mode))

(autoload-if-found '(quickrun) "quickrun" nil t)

(autoload-if-found '(restclient-mode) "restclient" nil t)

(with-eval-after-load 'smartparens)

(with-eval-after-load 'smart-jump)

(autoload-if-found '(string-inflection-all-cycle) "string-inflection" nil t)

(autoload-if-found '(sudo-edit-current-file) "sudo-edit" nil t)

(autoload-if-found '(topsy-mode) "topsy" nil t)
;; (with-eval-after-load 'prog-mode
;;   (add-hook 'prog-mode-hook #'topsy-mode))

;; (with-eval-after-load 'lsp-ui-mode
;;   (add-hook 'lsp-ui-mode-hook #'(lambda () (topsy-mode -1))))

(autoload-if-found '(uuid-string) "uuid" nil t)

(defun my/uuid ()
  (interactive)
  (insert (uuid-string)))

(defalias 'uuid #'my/uuid)

(autoload-if-found '(woman woman-find-file) "woman" nil t)

(autoload-if-found '(ace-window) "ace-window" nil t)

(keymap-global-set "C-x o" #'ace-window)

(with-eval-after-load 'ace-window
  (setq aw-dispatch-always t)
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-minibuffer-flag t))

(autoload-if-found '(writeroom-mode
                     writeroom-decrease-width
                     writeroom-increase-width
                     writeroom-adjust-width
                     writeroom-width)
                   "writeroom-mode" nil t)

(with-eval-after-load 'writeroom-mode
  ;; keybind
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width)

  ;; config
  (setq writeroom-width 150)
  (setq writeroom-maximize-window nil))

(autoload-if-found '(zoom-window-zoom) "zoom-window" nil t)

(keymap-global-set "C-c C-z" #'zoom-window-zoom)

(autoload-if-found '(docker-tramp-add-method) "docker-tramp" nil t)

;; (add-hook 'window-setup-hook #'docker-tramp-add-method)

(with-eval-after-load 'tramp
  (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist))

(autoload-if-found '(consult-tramp) "consult-tramp" nil t)

(autoload-if-found '(enable-paredit-mode
                     paredit-forward-slurp-sexp
                     paredit-splice-sexp
                     paredit-define-keys)
                   "paredit" nil t)

(keymap-global-set "C-c f" #'paredit-forward-slurp-sexp)
(keymap-global-set "M-s" #'paredit-splice-sexp)

(with-eval-after-load 'paredit
  (add-hook 'paredit-mode-hook #'paredit-define-keys))

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-data-mode-hook #'enable-paredit-mode))

(with-eval-after-load 'emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(with-eval-after-load 'lisp-interaction-mode
  (add-hook 'lisp-interacton-mode-hook #'enable-paredit-mode))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(with-eval-after-load 'simple
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode))

(with-eval-after-load 'ielm
  (add-hook 'inferior-emacs-lisp-mode-hook #'enable-paredit-mode))

(autoload-if-found '(rainbow-delimiters-mode-enable) "rainbow-delimiters" nil t)

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable))

(with-eval-after-load 'emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode-enable))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode-enable))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable))

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook
            #'(lambda ()
                (require 'slime)
                (require 'slime-autoloads)

                (load (expand-file-name "$HOME/.roswell/helper.el"))

                (defun my/slime-history ()
                  (interactive)
                  (if (and (fboundp '-distinct)
                           (fboundp 'f-read-text))
                      (insert
                       (completing-read
                        "choice history: "
                        (-distinct (read (f-read-text "~/.slime-history.eld"))))))))))

(with-eval-after-load 'slime
  (setq slime-net-coding-system 'utf-8-unix))

(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map (kbd "C-c C-r") #'my/slime-history))

(autoload-if-found '(hyperspec-lookup) "hyperspec" nil t)

(with-eval-after-load 'lisp-mode
  (define-key lisp-mode-map (kbd "C-c h") #'hyperspec-lookup))

(autoload-if-found '(eros-mode) "eros" nil t)

(with-eval-after-load 'emacs-lisp
  (add-hook 'emacs-lisp-mode-hook #'eros-mode))

(autoload-if-found '(turn-on-eldoc-mode) "eldoc" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode))

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode))

(autoload-if-found '() "trinary" nil t)

(autoload-if-found '(elsa-run) "elsa" nil t)

(autoload-if-found '(lispxmp) "lispxmp" nil t)

(autoload-if-found '(macrostep-expand macrostep-mode) "macrostep" nil t)

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand))

(autoload-if-found '(elisp-slime-nav-mode) "elisp-slime-nav" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode))

(autoload-if-found '(nameless-mode) "nameless" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'nameless-mode))

(autoload-if-found '(elisp-refs-function
                     elisp-refs-macro
                     elisp-refs-variable
                     elisp-refs-special
                     elisp-refs-symbol) "elisp-refs" nil t)

(autoload-if-found '(highlight-quoted-mode) "highlight-quoted" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(autoload-if-found '(highlight-defined-mode) "highlight-defined" nil t)

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

(autoload-if-found '(my/ielm-history) "ielm" nil t)

(defun my/ielm-history ()
  (interactive)
  (insert
   (completing-read
    "choice history: "
    (progn
      (let ((history nil)
            (comint-input-ring nil))
        (dotimes (index (ring-length comint-input-ring))
          (push (ring-ref comint-input-ring index) history))
        history)))))

(autoload-if-found '(anakondo-minor-mode) "anakondo" nil t)

;; (with-eval-after-load 'clojure-mode
;;   (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
;;   (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
;;   (add-hook 'clojurec-mode-hook #'anakondo-minor-mode))

(autoload-if-found '(cider cider-format-buffer cider-switch-to-last-clojure-buffer) "cider" nil t)
(autoload-if-found '(cider-doc) "cider-doc" nil t)

;; (add-hook 'before-save-hook #'cider-format-buffer t t)

(with-eval-after-load 'cider-common
  (setopt cider-special-mode-truncate-lines nil))

(with-eval-after-load 'cider-mode
  (setopt cider-font-lock-reader-conditionals nil)
  (setopt cider-font-lock-dynamically '(macro core function var)))

(with-eval-after-load 'cider-repl
  (setopt cider-repl-buffer-size-limit 1000000)
  (setopt cider-repl-wrap-history t)
  (setopt cider-repl-history-size 10000)
  (setopt cider-repl-tab-command #'indent-for-tab-command)
  (setopt cider-repl-display-in-current-window t))

(with-eval-after-load 'nrepl-client
  (setopt nrepl-use-ssh-fallback-for-remote-hosts t)
  (setopt nrepl-hide-special-buffers t))

(with-eval-after-load 'cider-eval
  (setopt cider-show-error-buffer nil)
  (setopt cider-auto-select-error-buffer nil))

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c h") #'cider-doc))

(autoload-if-found '(kibit kibit-current-file kibit-accept-proposed-change) "kibit-helper" nil t)

;; (autoload-if-found '(clj-refactor-mode cljr-add-keybindings-with-prefix) "clj-refactor" nil t)

;; (add-hook 'clojure-mode-hook #'clj-refactor-mode)
;; (cljr-add-keybindings-with-prefix "C-c C-m")

;; (with-eval-after-load 'clj-refactor
;;   (setopt cljr-suppress-middleware-warnings t)
;;   (setopt cljr-hotload-dependencies t))

(autoload-if-found '(inf-clojure) "inf-clojure" nil t)

(autoload-if-found '(clang-format-buffer) "clang-format" nil t)

(add-hook 'before-save-hook
          #'(lambda ()
              (when (member major-mode '(c-mode c++-mode))
                (clang-format-buffer))))

;; (autoload-if-found '(rainbow-csv-mode) "rainbow-csv" nil t)

;; (with-eval-after-load 'csv-mode
;;   (add-hook 'csv-mode-hook #'rainbow-csv-mode))

(autoload-if-found '(nodejs-repl
                     nodejs-repl-send-last-expression
                     nodejs-repl-send-line
                     nodejs-repl-send-region
                     nodejs-repl-send-buffer
                     nodejs-repl-load-file
                     nodejs-repl-switch-to-repl) "nodejs-repl" nil t)

(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-x C-e") #'nodejs-repl-send-last-expression)
  (define-key js2-mode-map (kbd "C-c C-j") #'nodejs-repl-send-line)
  (define-key js2-mode-map (kbd "C-c C-r") #'nodejs-repl-send-region)
  (define-key js2-mode-map (kbd "C-c C-c") #'nodejs-repl-send-buffer)
  (define-key js2-mode-map (kbd "C-c C-l") #'nodejs-repl-load-file)
  (define-key js2-mode-map (kbd "C-c C-z") #'nodejs-repl-switch-to-repl))

(autoload-if-found '(js2-refactor-mode) "js2-refactor" nil t)

(with-eval-after-load 'js2-refactor
  (setopt js2r-use-strict t))

(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook #'js2-refactor-mode))

(autoload-if-found '(jest
                     jest-file
                     jest-file-dwim
                     jest-function
                     jest-last-failed
                     jest-repeat
                     jest-minor-mode) "jest" nil t)

(with-eval-after-load 'typescript-mode
  ;; hook
  (add-hook 'typescript-mode-hook #'jest-minor-mode)

  ;; config
  (setopt jest-executable "npx jest"))

(with-eval-after-load 'projectile
  (projectile-register-project-type 'npx '("package.json" "yarn.lock")
                                    :project-file "package.json"
                                    :test "npx jest"
                                    :test-suffix ".spec"))

(autoload-if-found '(robe-mode inf-ruby-console-auto) "robe" nil t)

(autoload-if-found '(rubocop-mode) "rubocop" nil t)

(with-eval-after-load 'ruby-mode
  ;; config
  (setopt rubocop-keymap-prefix "C-c C-x")

  ;; hook
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(autoload-if-found '(ruby-refactor-mode-launch) "ruby-refactor" nil t)

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch))

(autoload-if-found '(inf-ruby inf-ruby-minor-mode) "inf-ruby" nil t)

(defun my/irb-history ()
  (interactive)
  (when (and (fboundp '-distinct)
             (fboundp 's-lines)
             (fboundp 'f-read-text))
    (insert
     (completing-read
      "choose history: "
      (mapcar #'list (-distinct (s-lines (f-read-text "~/.irb_history"))))))))

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(autoload-if-found '(yard-mode) "yard-mode" nil t)

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'yard-mode))

(autoload-if-found '(sqlind-setup sqlind-minor-mode) "sql-indent" nil t)

(with-eval-after-load 'sql
  (add-hook 'sql-mode-hook #'sqlind-setup)
  (add-hook 'sql-mode-hook #'sqlind-minor-mode))

(autoload-if-found '() "composer" nil t)

(autoload-if-found '(php-runtime-expr php-runtime-eval) "php-runtime" nil t)

(autoload-if-found '(psysh psysh-doc) "psysh" nil t)

(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c h") #'psysh-doc))

;; (autoload-if-found '(laravel-tinker-repl) "laravel-tinker-repl" nil t)

;; (with-eval-after-load 'php-mode
;;   (define-key php-mode-map (kbd "C-c C-c") #'laravel-tinker-repl-send-line)
;;   (define-key php-mode-map (kbd "C-c C-z") #'laravel-tinker-repl-switch-to-repl))

(autoload-if-found '(php-doc-block) "php-doc-block" nil t)

(autoload-if-found '(phpstan-analyze-file phpstan-analyze-this-file) "phpstan" nil t)

(defun my/flycheck-phpstan-setup ()
  "Setup Flycheck with PHPStan."
  (require 'flycheck-phpstan))

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook #'my/flycheck-phpstan-setup))

(with-eval-after-load 'php-ts-mode
  (add-hook 'php-ts-mode-hook #'my/flycheck-phpstan-setup))

(with-eval-after-load 'phpstan
  (setopt phpstan-memory-limit "4G"))

(autoload-if-found '(phpunit-current-test
                     phpunit-current-class
                     phpunit-current-project
                     phpunit-group) "phpunit" nil t)

(autoload-if-found '(poly-markdown-mode) "poly-markdown" nil t)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

(autoload-if-found '(markdown-preview-open-browser markdown-preview-mode) "markdown-preview-mode" nil t)

(with-eval-after-load 'markdown-preview-mode
  (setopt markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css")))

(autoload-if-found '(fish-repl) "fish-repl" nil t)

(autoload-if-found '(hindent-mode) "hindent" nil t)

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'hindent-mode))

(autoload-if-found '(emmet-mode) "emmet-mode" nil t)

(with-eval-after-load 'html-mode
  (add-hook 'html-mode-hook #'emmet-mode))

(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'emmet-mode))

(with-eval-after-load 'css-mode
  (add-hook 'css-mode-hook #'emmet-mode))

(with-eval-after-load 'nxml-mode
  (add-hook 'nxml-mode-hook #'emmet-mode))

(with-eval-after-load 'web-php-blade-mode
  (add-hook 'web-php-blade-mode #'emmet-mode))

(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-tsx-mode-hook #'emmet-mode))

(with-eval-after-load 'vue-mode
  (add-hook 'vue-mode-hook #'emmet-mode))

(with-eval-after-load 'emmet-mode
  ;; keybind
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "M-j") #'emmet-expand-line)

  ;; config
  (setopt emmet-self-closing-tag-style "")
  (setopt emmet-indent-after-insert nil))

(autoload-if-found '(jq-interactively) "jq-mode" nil t)

(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))

(autoload-if-found '(json-reformat-region) "json-reformat" nil t)

(autoload-if-found '(py-isort-region
                     py-isort-buffer
                     py-isort-before-save) "py-isort" nil t)

(autoload-if-found '(elfeed) "elfeed" nil t)

(with-eval-after-load 'elfeed-show
  (setopt elfeed-show-entry-switch #'display-buffer))

(with-eval-after-load 'elfeed-db
  (setopt elfeed-db-directory "~/.cache/elfeed"))

(with-eval-after-load 'elfeed-search
  (setopt elfeed-search-filter "@2-week-ago"))

(with-eval-after-load 'elfeed-curl
  (setopt elfeed-curl-max-connections 32))

(autoload-if-found '(elfeed-org) "elfeed-org" nil t)

(add-hook 'window-setup-hook #'elfeed-org)

(with-eval-after-load 'elfeed-org
  (setq rmh-elfeed-org-files '("~/.ghq/github.com/takeokunn/private/elfeed.org"))
  (setq rmh-elfeed-org-auto-ignore-invalid-feeds t))

(autoload-if-found '(elfeed-dashboard) "elfeed-dashboard" nil t)

(keymap-global-set "C-x w" #'elfeed-dashboard)

(with-eval-after-load 'elfeed-dashboard
  (setopt elfeed-dashboard-file (locate-user-emacs-file "elfeed-dashboard.org")))

(autoload-if-found '(elfeed-goodies/setup) "elfeed-goodies" nil t)

(with-eval-after-load 'elfeed-dashboard
  (add-hook 'elfeed-dashboard-mode-hook #'elfeed-goodies/setup))

(with-eval-after-load 'elfeed-goodies
  (setopt elfeed-goodies/entry-pane-size 0.6))

(with-eval-after-load 'org
  ;; keybind
  (define-key org-mode-map (kbd "C-c ,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "C-c C-,") #'org-insert-structure-template)

  ;; directory
  (setopt org-directory "~/.ghq/github.com/takeokunn/private")

  ;; todo
  (setopt org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "WAIT(w)" "|" "DONE(d)")))

  ;; startup
  (setopt org-startup-folded 'show3levels)
  (setopt org-startup-truncated nil)
  (setopt org-src-window-setup 'current-window)

  ;; archive
  (advice-add 'org-archive-subtree :before #'(lambda (&rest _) (remove-hook 'find-file-hooks #'view-mode)))
  (advice-add 'org-archive-subtree :after #'(lambda (&rest _) (add-hook 'find-file-hooks #'view-mode)))

  (defvar my/org-agenda-files `(,(concat org-directory "/agenda")
                                ,(concat org-directory "/archive/2023")
                                ,(concat org-directory "/archive/2024")))

  (setopt org-agenda-files my/org-agenda-files)
  (setopt org-archive-location `,(format (expand-file-name "archive/%s/%s.org::* Archived Tasks" org-directory)
                                       (format-time-string "%Y" (current-time))
                                       (format-time-string "%Y-%m-%d" (current-time))))

  ;; log
  (setopt org-log-into-drawer t)
  (setopt org-log-done 'time)

  (defun my/update-org-agenda-files ()
    (interactive)
    (setopt org-agenda-files my/org-agenda-files)))

(with-eval-after-load 'org-clock
  ;; hooks
  (add-hook 'org-mode-hook #'org-clock-load)
  (add-hook 'kill-emacs-hook #'org-clock-save)

  ;; config
  (setopt org-clock-out-remove-zero-time-clocks t)
  (setopt org-clock-clocked-in-display 'mode-line))

(with-eval-after-load 'org-list
  (setopt org-list-allow-alphabetical t))

(with-eval-after-load 'org-keys
  (setopt org-use-extra-keys t)
  (setopt org-use-speed-commands t)

  (add-to-list 'org-speed-commands '("d" org-todo "DONE"))
  ;; (add-to-list 'org-speed-commands '("j" call-interactively #'consult-org-heading))
  )

(autoload-if-found '(org-capture) "org-capture" nil t)
(keymap-global-set "C-c c" #'org-capture)

(add-hook 'window-setup-hook #'(lambda ()
                                 (advice-add 'org-capture :before #'(lambda (&rest _) (remove-hook 'find-file-hooks #'view-mode)))
                                 (advice-add 'org-capture :after #'(lambda (&rest _) (add-hook 'find-file-hooks #'view-mode)))))

(with-eval-after-load 'org-capture
  (setopt org-capture-use-agenda-date t)
  (setopt org-capture-bookmark nil)
  (setopt org-capture-templates `(("t" "Todo" entry (file ,(expand-file-name "todo.org" org-directory))
                                   "* %?")
                                  ("m" "Memo" entry (file ,(expand-file-name "memo.org" org-directory))
                                   "* %?")
                                  ("j" "Journal" entry (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
                                   "* %U\n%?\n%i\n"))))

(with-eval-after-load 'org-duration
 (setopt org-duration-format (quote h:mm)))

(autoload-if-found '(org-id-store-link) "org-id" nil t)

(with-eval-after-load 'org-id
  (setopt org-id-locations-file (expand-file-name ".org-id-locations" org-directory))
  (setopt org-id-extra-files (append org-agenda-text-search-extra-files)))

(autoload-if-found '(org-encrypt-entry org-decrypt-entry org-crypt-use-before-save-magic) "org-crypt" nil t)

(add-hook 'window-setup-hook #'org-crypt-use-before-save-magic)

(with-eval-after-load 'org-crypt
  (setopt org-crypt-key nil)
  (setopt org-tags-exclude-from-inheritance '("crypt")))

(autoload-if-found '(orgtbl-mode org-table-begin org-table-end) "org-table" nil t)

(with-eval-after-load 'org
  (defun my/org-table-align-markdown ()
    "Replace \"+\" sign with \"|\" in org-table."
    (when (member major-mode '(markdown-mode))
      (save-excursion
        (save-restriction
          (narrow-to-region (org-table-begin) (org-table-end))
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-"))))))
  (advice-add 'org-table-align :before #'my/org-table-align-markdown))

(with-eval-after-load 'org-journal
  (setopt org-journal-dir (expand-file-name "journal" org-directory))
  (setopt org-journal-start-on-weekday 7)
  (setopt org-journal-prefix-key "C-c j"))

(autoload-if-found '(org-generate) "org-generate" nil t)

(autoload-if-found '(org-pomodoro) "org-pomodoro" nil t)

(autoload-if-found '(org-view-mode) "org-view-mode" nil t)

(autoload-if-found '(org-random-todo org-random-todo-goto-current) "org-random-todo" nil t)

(autoload-if-found '(org-projectile-todo-files
                     org-projectile-project-todo-completing-read)
                   "org-projectile" nil t)

(keymap-global-set "C-c n p" #'org-projectile-project-todo-completing-read)

(with-eval-after-load 'org
  (setopt org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(autoload-if-found '(org-dashboard-display) "org-dashboard" nil t)

(autoload-if-found '(org-volume-update-entry-from-dblock) "org-volume" nil t)

(autoload-if-found '(org-ql-query org-ql-select) "org-ql" nil t)

(with-eval-after-load 'org-faces
  (setopt org-link '(t (:foreground "#ebe087" :underline t))))

(autoload-if-found '(org-superstar-mode) "org-superstar")

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-superstar-mode))

(with-eval-after-load 'org-superstar
  (setopt org-superstar-headline-bullets-list '("◉" "○" "✸" "✿"))
  (setopt org-superstar-leading-bullet " "))

(autoload-if-found '(toc-org-mode) "toc-org" nil t)

(with-eval-after-load 'org
  ;; hook
  (add-hook 'org-mode-hook #'toc-org-mode))

(autoload-if-found '(org-tree-slide-mode org-tree-slide-skip-done-toggle) "org-tree-slide" nil t)

(with-eval-after-load 'org-tree-slide
  (setopt org-tree-slide-skip-outline-level 4))

(autoload-if-found '(org-store-link) "ol" nil t)

(keymap-global-set "C-c l" #'org-store-link)

(with-eval-after-load 'ol
  (setopt org-link-file-path-type 'relative))

;; (eval-when-compile
;;   (el-clone :repo "emacsmirror/org-link-beautify"))
;;
;; (with-delayed-execution
;;   (message "Install org-link-beautify...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/org-link-beautify"))
;;
;;   (autoload-if-found '(org-link-beautify-mode) "org-link-beautify" nil t)
;;
;;   ;; (with-eval-after-load 'org
;;   ;;   (add-hook 'org-mode-hook #'org-link-beautify-mode))
;;   )

;; (eval-when-compile
;;   (el-clone :repo "magit/orgit"))
;;
;; (with-delayed-execution
;;   (message "Install orgit...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/orgit"))
;;
;;   (autoload-if-found '(orgit-store-link) "orgit" nil t)
;;
;;   (with-eval-after-load 'magit
;;     (define-key magit-mode-map [remap org-store-link] #'orgit-store-link)))

(autoload-if-found '(org-agenda) "org-agenda" nil t)

(keymap-global-set "C-c a" #'org-agenda)

(with-eval-after-load 'org-agenda
  (setopt org-agenda-span 'day)
  (setopt org-agenda-start-on-weekday 1)
  (setopt org-agenda-todo-ignore-with-date t))

(autoload-if-found '(org-super-agenda-mode) "org-super-agenda" nil t)

(add-hook 'window-setup-hook #'org-super-agenda-mode)

(with-eval-after-load 'org-super-agenda
  (setopt org-super-agenda-groups '((:log t)
                                    (:auto-group t)
                                    (:name "Today List..." :scheduled today)
                                    (:name "Due Today List..." :deadline today)
                                    (:name "Overdue List..." :deadline past)
                                    (:name "Due Soon List" :deadline future)
                                    (:name "TODO List..." :todo "TODO")
                                    (:name "WAIT List..." :todo "WAIT")
                                    (:name "DONE List..." :todo "DONE")
                                    (:name "SOMEDAY List..." :todo "SOMEDAY"))))

(autoload-if-found '(org-hyperscheduler-open) "org-hyperscheduler" nil t)

(autoload-if-found '(org-timeblock-list org-timeblock-mode) "org-timeblock" nil t)

(autoload-if-found '(org-redmine-get-issue) "org-redmine" nil t)

(with-eval-after-load 'org-redmine
  (setopt org-redmine-template-header "[#%i%] %s%")
  (setopt org-redmine-template-property '(("project_name" . "%p_n%"))))

(autoload-if-found '(org-ai-mode) "org-ai" nil t)

(with-eval-after-load 'org
  ;; config
  (add-to-list 'org-structure-template-alist '("A" . "ai"))

  ;; hook
  (add-hook 'org-mode-hook #'org-ai-mode))

(with-eval-after-load 'org-ai
  ;; config
  (setopt org-ai-default-chat-model "gpt-3.5-turbo"))

(autoload-if-found '(org-babel-do-load-languages) "org" nil t)

(add-hook 'window-setup-hook
          #'(lambda ()
              (org-babel-do-load-languages 'org-babel-load-languages
                                           '((awk . t)
                                             (C . t)
                                             (R . t)
                                             (clojure . t)
                                             (emacs-lisp . t)
                                             (haskell . t)
                                             (java . t)
                                             (js . t)
                                             (lisp . t)
                                             (makefile . t)
                                             (perl . t)
                                             (plantuml . t)
                                             (python . t)
                                             (ruby . t)
                                             (scheme . t)
                                             (shell . t)
                                             (sql . t)
                                             (shell . t)))))

(with-eval-after-load 'ob-core
  (setopt org-confirm-babel-evaluate nil)

  (add-to-list 'org-babel-default-header-args '(:results . "output")))

(with-eval-after-load 'ob-eval
  (advice-add #'org-babel-eval-error-notify
              :around #'(lambda (old-func &rest args)
                          (when (not (string= (nth 1 args)
                                              "mysql: [Warning] Using a password on the command line interface can be insecure.\n"))
                            (apply old-func args)))))

(autoload-if-found '(ob-async-org-babel-execute-src-block) "ob-async" nil t)

;; (advice-add 'org-babel-execute-src-block :around #'ob-async-org-babel-execute-src-block)

;; (autoload-if-found '(org-babel-execute:fish) "ob-fish" nil t)

;; (with-eval-after-load 'org-src
;;   (add-to-list 'org-src-lang-modes '("fish" . fish)))

(autoload-if-found '(org-babel-execute:rust) "ob-rust" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("rust" . rust)))

(autoload-if-found '(org-babel-execute:go) "ob-go" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("go" . go)))

(autoload-if-found '(ob-translate:google-translate) "ob-translate" nil t)

(with-eval-after-load 'text-mode
  (define-derived-mode translate-mode text-mode "translate"))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("translate" . translate)))

(autoload-if-found '(org-babel-execute:typescript) "ob-typescript" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("typescript" . typescript)))

(autoload-if-found '(org-babel-execute:php) "ob-php" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("php" . php)))

(autoload-if-found '(org-babel-execute:phpstan) "ob-phpstan" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("phpstan" . phpstan)))

(autoload-if-found '(org-babel-execute:http) "ob-http" nil t)
(autoload-if-found '(ob-http-mode) "ob-http-mode" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("http" . ob-http)))

(autoload-if-found '(org-babel-execute:mermaid) "ob-mermaid" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid)))

(autoload-if-found '(org-babel-execute:graphql) "ob-graphql" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("graphql" . graphql)))

(autoload-if-found '(org-babel-execute:rust) "ob-rust" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("rust" . rust)))

(autoload-if-found '(org-babel-execute:swift) "ob-swift" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("swift" . swift)))

(autoload-if-found '(org-babel-execute:elixir) "ob-elixir" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("elixir" . elixir)))

(autoload-if-found '(org-babel-execute:dart) "ob-dart" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("dart" . dart)))

(autoload-if-found '(org-babel-execute:fsharp) "ob-fsharp" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("fsharp" . fsharp)))

(autoload-if-found '(org-babel-execute:treesitter) "ob-treesitter" nil t)

(with-eval-after-load 'prog-mode
  (define-derived-mode treesitter-mode prog-mode "treesitter"))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("treesitter" . treesitter)))

(autoload-if-found '(org-babel-execute:base64) "ob-base64" nil t)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("base64" . base64)))

(autoload-if-found '(org-nix-shell-mode) "org-nix-shell" nil t)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-nix-shell-mode))

(with-eval-after-load 'ox-html
  (setopt org-html-head-include-default-style nil)
  (setopt org-html-head-include-scripts nil)
  (setopt org-html-doctype "html5")
  (setopt org-html-coding-system 'utf-8-unix))

(autoload-if-found '(org-gfm-export-as-markdown
                       org-gfm-convert-region-to-md
                       org-gfm-export-to-markdown
                       org-gfm-publish-to-gfm) "ox-gfm" nil t)

(autoload-if-found '(org-zenn-export-as-markdown
                     org-zenn-export-to-markdown
                     org-zenn-publish-to-markdown
                     org-zenn-convert-region-to-md) "ox-zenn" nil t)

(autoload-if-found '(org-hatena-export-as-hatena
                     org-hatena-export-to-hatena
                     org-hatena-export-to-hatena-and-open) "ox-hatena" nil t)

(autoload-if-found '(org-qmd-export-as-markdown
                     org-qmd-convert-region-to-md
                     org-qmd-export-to-markdown) "ox-qmd" nil t)

(autoload-if-found '(org-hugo-export-as-md
                     org-hugo-export-to-md
                     org-hugo-export-wim-to-md
                     org-hugo-debug-info) "ox-hugo" nil t)

(with-eval-after-load 'ox-hugo
  (setopt org-hugo-auto-set-lastmod t))

(autoload-if-found '(org-roam-graph) "org-roam" nil t)

(keymap-global-set "C-c n g" #'org-roam-graph)

(with-eval-after-load 'org-roam
  (setopt org-roam-directory `,(concat (s-trim-right (shell-command-to-string "ghq root"))
                                       "/github.com/takeokunn/blog")))

(autoload-if-found '(org-roam-buffer-toggle) "org-roam-mode" nil t)

(keymap-global-set "C-c n l" #'org-roam-buffer-toggle)

(autoload-if-found '(org-roam-node-find org-roam-node-insert) "org-roam-node" nil t)

(keymap-global-set "C-c n f" #'org-roam-node-find)
(keymap-global-set "C-c n i" #'org-roam-node-insert)

(with-eval-after-load 'org-roam-node
  (setopt org-roam-completion-everywhere nil))

(autoload-if-found '(org-roam-db-autosync-enable) "org-roam-db" nil t)

(add-hook 'window-setup-hook #'org-roam-db-autosync-enable)

(with-eval-after-load 'org-roam-db
  (setq org-roam-database-connector 'sqlite)
  (setq org-roam-db-gc-threshold (* 4 gc-cons-threshold)))

(autoload-if-found '(org-roam-capture) "org-roam-capture" nil t)

(keymap-global-set "C-c n c" #'org-roam-capture)

(with-eval-after-load 'org-roam-capture
  (setopt org-roam-capture-templates '(("f" "Fleeting(一時メモ)" plain "%?"
                                        :target (file+head "org/fleeting/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("l" "Literature(文献)" plain "%?"
                                        :target (file+head "org/literature/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("p" "Permanent(記事)" plain "%?"
                                        :target (file+head "org/permanent/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("d" "Diary(日記)" plain "%?"
                                        :target (file+head "org/diary/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("z" "Zenn" plain "%?"
                                        :target (file+head "org/zenn/%<%Y%m%d%H%M%S>.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("m" "Private" plain "%?"
                                        :target (file+head "org/private/%<%Y%m%d%H%M%S>.org.gpg" "#+TITLE: ${title}\n")
                                        :unnarrowed t)
                                       ("o" "Poem" plain "%?"
                                        :target (file+head "org/poem/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                        :unnarrowed t))))

(autoload-if-found '(org-roam-dailies-map
                     org-roam-dailies-goto-today
                     org-roam-dailies-goto-yesterday
                     org-roam-dailies-goto-tomorrow
                     org-roam-dailies-capture-today
                     org-roam-dailies-goto-next-note
                     org-roam-dailies-goto-previous-note
                     org-roam-dailies-goto-date
                     org-roam-dailies-capture-date
                     org-roam-dailies-find-directory) "org-roam-dailies" nil t)

(keymap-global-set "C-c n d" #'org-roam-dailies-map)
(keymap-global-set "C-c n j" #'org-roam-dailies-goto-today)

(with-eval-after-load 'org-roam-dailies
  ;; config
  (setopt org-roam-dailies-directory "org/daily/")

  ;; keybind
  (define-key org-roam-dailies-map (kbd "d") #'org-roam-dailies-goto-today)
  (define-key org-roam-dailies-map (kbd "y") #'org-roam-dailies-goto-yesterday)
  (define-key org-roam-dailies-map (kbd "t") #'org-roam-dailies-goto-tomorrow)
  (define-key org-roam-dailies-map (kbd "n") #'org-roam-dailies-capture-today)
  (define-key org-roam-dailies-map (kbd "f") #'org-roam-dailies-goto-next-note)
  (define-key org-roam-dailies-map (kbd "b") #'org-roam-dailies-goto-previous-note)
  (define-key org-roam-dailies-map (kbd "c") #'org-roam-dailies-goto-date)
  (define-key org-roam-dailies-map (kbd "v") #'org-roam-dailies-capture-date)
  (define-key org-roam-dailies-map (kbd ".") #'org-roam-dailies-find-directory))

(autoload-if-found '(org-roam-export--org-html--reference) "org-roam-export" nil t)

(add-hook 'window-setup-hook
          #'(lambda ()
              (advice-add 'org-html--reference :override #'org-roam-export--org-html--reference)))

(autoload-if-found '(org-roam-graph org-roam-graph--open) "org-roam-graph" nil t)

(autoload-if-found '(org-roam-overlay-mode) "org-roam-overlay" nil t)

(with-eval-after-load 'org-roam-mode
  (add-hook 'org-roam-mode-hook #'org-roam-overlay-mode))

(autoload-if-found '(org-roam-protocol-open-ref org-roam-protocol-open-node) "org-roam-protocol" nil t)

(with-eval-after-load 'org-roam-protocol
  ;; alist
  (add-to-list 'org-protocol-protocol-alist '("org-roam-ref" :protocol "roam-ref" :function org-roam-protocol-open-ref))
  (add-to-list 'org-protocol-protocol-alist '("org-roam-node" :protocol "roam-node" :function org-roam-protocol-open-node))

  ;; config
  (setopt org-roam-protocol-store-links t))

(autoload-if-found '(consult-org-roam-mode) "consult-org-roam" nil t)

(add-hook 'window-setup-hook #'consult-org-roam-mode)

;; keybinds
(keymap-global-set "C-c n e" #'consult-org-roam-file-find)
(keymap-global-set "C-c n b" #'consult-org-roam-backlinks)
(keymap-global-set "C-c n B" #'consult-org-roam-backlinks-recursive)
(keymap-global-set "C-c n l" #'consult-org-roam-forward-links)
(keymap-global-set "C-c n r" #'consult-org-roam-search)

(with-eval-after-load 'consult-org-roam
  (setopt consult-org-roam-grep-func #'consult-ripgrep)
  (setopt consult-org-roam-buffer-narrow-key ?r)
  (setopt consult-org-roam-buffer-after-buffers t))

(autoload-if-found '(org-roam-ui-mode) "org-roam-ui" nil t)

(with-eval-after-load 'org-roam-mode
  (add-hook 'org-roam-mode-hook #'org-roam-ui-mode))

(with-eval-after-load 'org-roam-ui
  (setopt org-roam-ui-sync-theme t)
  (setopt org-roam-ui-follow t)
  (setopt org-roam-ui-update-on-save t)
  (setopt org-roam-ui-open-on-start t))

(autoload-if-found '(org-roam-timestamps-mode) "org-roam-timestamps" nil t)

(with-eval-after-load 'org-roam-mode
  (add-hook 'org-roam-mode #'org-roam-timestamps-mode))

(with-eval-after-load 'org-roam-timestamps
  (setopt org-roam-timestamps-remember-timestamps nil))

(autoload-if-found '() "org-roam-search-node-insert" nil t)

;; (autoload-if-found '(org-roam-ql-nodes
;;                      org-roam-ql-search
;;                      org-roam-ql-defpred
;;                      org-roam-ql-agenda-buffer-from-roam-buffer
;;                      org-roam-ql-refresh-buffer
;;                      org-dblock-write:org-roam-ql) "org-roam-ql" nil t)
;; (autoload-if-found '(org-roam-ql-ql-init) "org-roam-ql-ql" nil t)

;; (add-hook 'window-setup-hook #'org-roam-ql-ql-init)

;; (eval-when-compile
;;   (el-clone :repo "ch11ng/exwm"))

;; (eval-when-compile
;;   (el-clone :repo "agzam/exwm-edit"))

;; (when (string= system-type "gnu/linux")
;;   (with-delayed-execution
;;     (message "Install exwm-edit...")
;;     (add-to-list 'load-path (locate-user-emacs-file "el-clone/exwm-edit"))

;;     (autoload-if-found '(exwm-edit--compose-minibuffer) "exwm-edit" nil t)

;;     (exwm-input-set-key (kbd "C-c '") #'exwm-edit--compose-minibuffer)
;;     (exwm-input-set-key (kbd "C-c C-'") #'exwm-edit--compose-minibuffer)

;;     (with-eval-after-load 'exwm-edit
;;       (setopt exwm-edit-bind-default-keys nil))))

;; (eval-when-compile
;;   (el-clone :repo "SqrtMinusOne/exwm-modeline"))

;; (when (string= system-type "gnu/linux")
;;   (with-delayed-execution
;;     (message "Install exwm-modeline...")
;;     (add-to-list 'load-path (locate-user-emacs-file "el-clone/exwm-modeline"))

;;     (autoload-if-found '(exwm-modeline-mode) "exwm-modeline")

;;     (with-eval-after-load 'exwm-core
;;       (add-hook 'exwm-mode-hook #'exwm-modeline-mode))

;;     (with-eval-after-load 'exwm-modeline
;;       (setopt exwm-modeline-short t))))

(autoload-if-found '(copilot-login
                     copilot-mode
                     global-copilot-mode) "copilot" nil t)

(with-eval-after-load 'copilot
  ;; config
  (setopt copilot-log-max 100000)

  ;; keymap
  (define-key copilot-mode-map (kbd "C-c # i") #'copilot-complete)
  (define-key copilot-mode-map (kbd "C-c # a") #'copilot-accept-completion))

(autoload-if-found '(make-llm-ollama) "llm-ollama" nil t)

(with-eval-after-load 'llm
  (setopt llm-warn-on-nonfree nil))

(autoload-if-found '(ellama-chat
                     ellama-ask-about
                     ellama-ask-line
                     ellama-complete
                     ellama-translate
                     ellama-define-word
                     ellama-summarize
                     ellama-code-review
                     ellama-change
                     ellama-enhance-grammar-spelling
                     ellama-enhance-wording
                     ellama-make-concise
                     ellama-change-code
                     ellama-enhance-code
                     ellama-complete-code
                     ellama-add-code
                     ellama-render
                     ellama-make-list
                     ellama-make-table
                     ellama-summarize-webpage
                     ellama-provider-select
                     ellama-code-complete
                     ellama-code-add
                     ellama-code-edit
                     ellama-code-improve
                     ellama-improve-wording
                     ellama-improve-grammar
                     ellama-improve-conciseness
                     ellama-make-format
                     ellama-ask-interactive)
                   "ellama" nil t)

(with-eval-after-load 'ellama
  (setopt ellama-language "日本語")
  (setopt ellama-translation-template "
Translate this text to %s.

Original text:
%s

Translation to %s:


以下のフォーマットで出力してください。

元テキスト:

<ここに元テキストを出力する>

翻訳後テキスト:

<ここに翻訳後テキストを出力する>

英語構文解説:

<ここに英語の構文解説を日本語でする>
")
  (setopt ellama-provider (progn
                            (make-llm-ollama :chat-model "gemma2:27b" :embedding-model "gemma2:27b"))))

(defun my/beginning-of-intendation ()
  "move to beginning of line, or indentation"
  (interactive)
  (back-to-indentation))

(defun my/copy-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (copy-region-as-kill (region-beginning) (region-end))))

(defalias 'copy-buffer 'my/copy-buffer)

(defun my/ghq-get ()
  (interactive)
  (let ((url (read-string "url > ")))
    (message
     (shell-command-to-string
      (mapconcat #'shell-quote-argument
                 (list "ghq" "get" url)
                 " ")))))

(defalias 'ghq-get 'my/ghq-get)

(defun my/gh-browse ()
  (interactive)
  (message
   (shell-command-to-string
    (mapconcat #'shell-quote-argument
               (list "gh" "browse")
               " "))))

(defalias 'gh-browse 'my/gh-browse)

(defun my/indent-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))))

(defalias 'indent-buffer 'my/indent-buffer)

(defun my/move-line (arg)
  (interactive)
  (let ((col (current-column)))
    (unless (eq col 0)
      (move-to-column 0))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (forward-line arg)))

(defun my/move-line-down ()
  (interactive)
  (my/move-line 1))

(defun my/move-line-up ()
  (interactive)
  (my/move-line -1))

(keymap-global-set "M-N" #'my/move-line-down)
(keymap-global-set "M-P" #'my/move-line-up)

(defun my/reload-major-mode ()
  "Reload current major mode."
  (interactive)
  (let ((current-mode major-mode))
    (fundamental-mode)
    (funcall current-mode)
    current-mode))

(defvar my/kill-emacs-keybind-p t)

(defun my/toggle-kill-emacs ()
  (interactive)
  (if my/kill-emacs-keybind-p
      (progn
        (message "C-x C-c save-buffers-kill-emacs OFF")
        (setq my/kill-emacs-keybind-p nil)
        (keymap-global-set "C-x C-c" nil))
    (progn
      (message "C-x C-c save-buffers-kill-emacs ON")
      (setq my/kill-emacs-keybind-p t)
      (keymap-global-set "C-x C-c" 'save-buffers-kill-emacs))))

(defun my/get-class-name-by-file-name ()
  (interactive)
  (insert
   (file-name-nondirectory
    (file-name-sans-extension (or (buffer-file-name)
                                  (buffer-name (current-buffer)))))))

(defun my/insert-clipboard (arg)
  (interactive "sstring: ")
  (kill-new arg))

(defun my/actionlint ()
  (interactive)
  (shell-command-to-string "actionlint"))

(defalias 'actionlint 'my/actionlint)

(defun my/build-info ()
  "Display build information in a buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Build info*"))
  (setq tab-width 4)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert (format "GNU Emacs %s\nCommit:\t\t%s\nBranch:\t\t%s\n"
                    emacs-version
                    emacs-repository-version
                    emacs-repository-branch))
    (insert (format "System:\t\t%s\nDate:\t\t%s\n"
                    system-configuration
                    (format-time-string "%Y-%m-%d %T (%Z)" emacs-build-time)))
    (insert (format "Patch:\t\t%s ns-inline.patch\n"
                    (if (boundp 'mac-ime--cursor-type) "with" "without")))
    (insert (format "Features:\t%s\n" system-configuration-features))
    (view-mode)))

(defun my/current-ip-address ()
  (interactive)
  (insert
   (shell-command-to-string "curl -s ifconfig.me")))

(defun my/today ()
  (interactive)
  (insert
   (format-time-string "%Y-%m-%d %a" (current-time))))

(defalias 'today 'my/today)

(setq file-name-handler-alist my/saved-file-name-handler-alist)

(when my/enable-profile
  (profiler-report)
  (profiler-stop))