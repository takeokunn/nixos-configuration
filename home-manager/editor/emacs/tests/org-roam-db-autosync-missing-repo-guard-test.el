;;; org-roam-db-autosync-missing-repo-guard-test.el --- Org-roam autosync guard regression test -*- lexical-binding: t; -*-

;; init.org's "org-roam-db" section hooks Org-roam autosync through
;; `my/org-roam-db-autosync-maybe-enable', which skips autosync when the
;; github.com/takeokunn/blog checkout is missing.  Unguarded, the autosync
;; scan of `org-roam-directory' signals file-missing on every Org buffer
;; visit when that repository has never been cloned.
;;
;; The defun is read out of init.org rather than restated here, so these
;; tests fail if the guard is removed or loses its directory check.  Its two
;; dependencies are stubbed: `my/ghq-repo-path' shells out to ghq, and
;; `org-roam-db-autosync-enable' needs the org-roam package.
;;
;; Run from the repository root:
;;
;;   emacs --batch -l home-manager/editor/emacs/tests/org-roam-db-autosync-missing-repo-guard-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

(defconst org-roam-db-autosync-guard-test--init-org
  (expand-file-name "../elisp/init.org"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun org-roam-db-autosync-guard-test--init-org-text ()
  "Return the contents of init.org as a string."
  (with-temp-buffer
    (insert-file-contents org-roam-db-autosync-guard-test--init-org)
    (buffer-string)))

(defun org-roam-db-autosync-guard-test--load-guard ()
  "Evaluate the `my/org-roam-db-autosync-maybe-enable' defun from init.org."
  (with-temp-buffer
    (insert (org-roam-db-autosync-guard-test--init-org-text))
    (goto-char (point-min))
    (unless (search-forward "(defun my/org-roam-db-autosync-maybe-enable" nil t)
      (error "my/org-roam-db-autosync-maybe-enable is not defined in %s"
             org-roam-db-autosync-guard-test--init-org))
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(defun org-roam-db-autosync-guard-test--autosync-invoked-p (repo-path)
  "Return non-nil if the guard calls autosync when the repo resolves to REPO-PATH."
  (org-roam-db-autosync-guard-test--load-guard)
  (let (autosync-invoked)
    (cl-letf (((symbol-function 'my/ghq-repo-path)
               (lambda (_relative) repo-path))
              ((symbol-function 'org-roam-db-autosync-enable)
               (lambda () (setq autosync-invoked t))))
      (my/org-roam-db-autosync-maybe-enable))
    autosync-invoked))

(ert-deftest org-roam-db-autosync-guard/enables-when-repo-checked-out ()
  (should (org-roam-db-autosync-guard-test--autosync-invoked-p
           temporary-file-directory)))

(ert-deftest org-roam-db-autosync-guard/skips-when-repo-missing ()
  (should-not (org-roam-db-autosync-guard-test--autosync-invoked-p
               (make-temp-name
                (expand-file-name "org-roam-guard-missing-"
                                  temporary-file-directory)))))

(ert-deftest org-roam-db-autosync-guard/org-mode-hook-uses-guard ()
  (let ((text (org-roam-db-autosync-guard-test--init-org-text)))
    (should (string-search
             "(add-hook 'org-mode-hook #'my/org-roam-db-autosync-maybe-enable)"
             text))
    (should-not (string-search
                 "(add-hook 'org-mode-hook #'org-roam-db-autosync-enable)"
                 text))))

(provide 'org-roam-db-autosync-missing-repo-guard-test)

;;; org-roam-db-autosync-missing-repo-guard-test.el ends here
