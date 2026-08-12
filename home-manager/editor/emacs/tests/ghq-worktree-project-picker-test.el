;;; ghq-worktree-project-picker-test.el --- ERT regression tests -*- lexical-binding: t; -*-

;; Guards the ghq worktree/project-picker additions made to
;; home-manager/editor/emacs/elisp/init.org (the "** projectile" and
;; "*** magit" sections): `my/ghq-repo-list', `my/ghq--git',
;; `my/ghq-repo-worktrees', `my/ghq--resolve-base-ref', `my/ghq--link-state',
;; `my/ghq-create-worktree', `my/ghq-switch-project', the roots-computed-once
;; refactor of `my/update-projectile-known-projects' /
;; `my/update-ghq-project-state', and `my/update-magit-repository-directories'.
;;
;; Invocation (run from the repo root; requires the repo's Nix/
;; home-manager-built `emacs' and `git' on PATH):
;;
;;   emacs --batch -l home-manager/editor/emacs/tests/ghq-worktree-project-picker-test.el -f ert-run-tests-batch-and-exit
;;
;; Extraction strategy
;; -------------------
;; init.org is a literate-config file tangled by org-babel, not a loadable
;; .el file, and its "** projectile" / "*** magit" #+begin_src blocks also
;; contain `autoload-if-found', `add-hook', `keymap-global-set', and
;; `with-eval-after-load' forms that assume projectile/magit/consult are
;; already on the load path -- which this narrow, dependency-free suite
;; deliberately does not require.
;;
;; Rather than hand-copying the function bodies into this file (which would
;; silently drift from init.org the next time either file is edited without
;; the other), this file reads init.org itself at test-run time, locates the
;; "** projectile" and "*** magit" #+begin_src ... #+end_src blocks by a
;; plain, line-anchored string search, and pulls out only the top-level
;; `defun' forms whose name is in the target list below. Those forms are
;; `eval'd verbatim -- byte-for-byte what init.org currently contains -- and
;; nothing else in either block (the autoload/hook/keymap/with-eval-after-load
;; forms) is ever evaluated, so no projectile/magit/consult dependency is
;; pulled in. This mirrors the tangled-buffer-extraction approach used
;; elsewhere in this repo for the same "one file, two problems" situation.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst test--init-org-path
  (expand-file-name "../elisp/init.org"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the literate config this suite extracts definitions from.")

(defun test--org-src-block (heading-regexp)
  "Return the raw text of the first #+begin_src ... #+end_src block that
follows the first line in `test--init-org-path' matching HEADING-REGEXP.
Read from disk fresh on every call, so the extraction always reflects
whatever init.org currently contains."
  (with-temp-buffer
    (insert-file-contents test--init-org-path)
    (goto-char (point-min))
    (unless (re-search-forward heading-regexp nil t)
      (error "test--org-src-block: heading not found: %s" heading-regexp))
    (unless (re-search-forward "^#\\+begin_src emacs-lisp :tangle yes$" nil t)
      (error "test--org-src-block: no source block after heading: %s" heading-regexp))
    (forward-line 1)
    (let ((start (point)))
      (unless (re-search-forward "^#\\+end_src$" nil t)
        (error "test--org-src-block: unterminated source block after heading: %s" heading-regexp))
      (buffer-substring-no-properties start (match-beginning 0)))))

(defun test--defuns-from-block (block-text names)
  "Read top-level forms out of BLOCK-TEXT and return the `(defun NAME ...)'
forms whose NAME is a member of NAMES, in the order they appear in
BLOCK-TEXT."
  (let ((pos 0) (len (length block-text)) forms)
    (while (progn
             (while (and (< pos len) (memq (aref block-text pos) '(?\s ?\t ?\n ?\f)))
               (setq pos (1+ pos)))
             (< pos len))
      (let* ((result (read-from-string block-text pos len))
             (form (car result)))
        (setq pos (cdr result))
        (when (and (consp form) (eq (car form) 'defun) (memq (cadr form) names))
          (push form forms))))
    (nreverse forms)))

(defconst test--projectile-defun-names
  '(my/ghq-bare-repository-p
    my/ghq-project-roots
    my/ghq-repo-list
    my/ghq--git
    my/ghq-repo-worktrees
    my/ghq--resolve-base-ref
    my/ghq--link-state
    my/ghq-create-worktree
    my/ghq-switch-project
    my/update-projectile-known-projects
    my/update-ghq-project-state))

(defconst test--magit-defun-names
  '(my/update-magit-repository-directories))

(dolist (form (append
               (test--defuns-from-block
                (test--org-src-block "^\\*\\* projectile$")
                test--projectile-defun-names)
               (test--defuns-from-block
                (test--org-src-block "^\\*\\*\\* magit$")
                test--magit-defun-names)))
  (eval form t))

;; Sanity check on the extraction itself: fail loudly, rather than with a
;; cryptic void-function error from inside the first test, if init.org's
;; headings or function names ever drift out from under this file.
(dolist (name (append test--projectile-defun-names test--magit-defun-names))
  (unless (fboundp name)
    (error "ghq-worktree-project-picker-test: failed to extract `%s' from init.org" name)))

;; --- Dependency-free stand-ins for the projectile/magit surface these
;; --- defuns touch. Only defined when the real package is not already
;; --- loaded (batch mode with no init file, so normally never), and always
;; --- overridden per-test via `cl-letf' where the test cares about calls
;; --- into them -- these top-level definitions exist solely so `cl-letf'
;; --- has a prior value to save and restore.
(unless (fboundp 'projectile-clear-known-projects)
  (defalias 'projectile-clear-known-projects #'ignore))
(unless (fboundp 'projectile-cleanup-known-projects)
  (defalias 'projectile-cleanup-known-projects #'ignore))
(unless (fboundp 'projectile-switch-project-by-name)
  (defalias 'projectile-switch-project-by-name #'identity))
(unless (boundp 'projectile-known-projects)
  (defcustom projectile-known-projects nil "Test stand-in." :type 'sexp))
(unless (boundp 'magit-repository-directories)
  (defcustom magit-repository-directories nil "Test stand-in." :type 'sexp))

;;; --- Fixture helpers -------------------------------------------------

(defmacro test--with-tmpdir (var &rest body)
  "Bind VAR to a fresh temp directory's truename for BODY, deleting it
recursively afterward even if BODY signals."
  (declare (indent 1))
  `(let ((,var (file-truename (make-temp-file "ghq-wt-test-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro test--with-tmpdirs (vars &rest body)
  "Bind each symbol in VARS to its own fresh temp directory truename for
BODY, deleting all of them recursively afterward even if BODY signals."
  (declare (indent 1))
  `(let* (,@(mapcar (lambda (v) `(,v (file-truename (make-temp-file "ghq-wt-test-" t)))) vars))
     (unwind-protect
         (progn ,@body)
       (dolist (d (list ,@vars))
         (when (file-directory-p d)
           (delete-directory d t))))))

(defun test--git (dir &rest args)
  "Run git ARGS in DIR via a direct `call-process', independent of
`my/ghq--git' (the function under test), so fixture arrangement never goes
through the seam a test is verifying. Returns trimmed stdout; signals an
error carrying the captured output on nonzero exit, so a broken fixture
never silently no-ops."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil "-C" dir args)))
      (unless (zerop exit)
        (error "test--git: `git %s' in %s failed (exit %s): %s"
               (string-join args " ") dir exit (buffer-string)))
      (string-trim (buffer-string)))))

(defun test--make-bare-repo-with-main (bare-dir)
  "Turn the already-existing empty directory BARE-DIR into a bare git
repository with an initial branch named \"main\" carrying one empty commit,
and no `origin' remote configured inside BARE-DIR itself. The throwaway
clone used to create the commit is discarded before this function returns,
so nothing about it leaks into BARE-DIR's own git config."
  (test--git bare-dir "init" "--bare" "-q" "-b" "main")
  (let ((clone-dir (file-truename (make-temp-file "ghq-wt-test-clone-" t))))
    (unwind-protect
        (progn
          (test--git (file-name-directory (directory-file-name clone-dir))
                     "clone" "-q" bare-dir clone-dir)
          (test--git clone-dir "config" "user.email" "test@example.com")
          (test--git clone-dir "config" "user.name" "Test")
          (test--git clone-dir "commit" "-q" "--allow-empty" "-m" "init")
          (test--git clone-dir "push" "-q" "origin" "main"))
      (when (file-directory-p clone-dir)
        (delete-directory clone-dir t)))))

;;; --- 1. my/ghq-bare-repository-p --------------------------------------

(ert-deftest ghq-bare-repository-p/true-without-dot-git-entry ()
  (test--with-tmpdir dir
    (should (my/ghq-bare-repository-p dir))))

(ert-deftest ghq-bare-repository-p/nil-with-dot-git-entry ()
  (test--with-tmpdir dir
    (make-directory (expand-file-name ".git" dir) t)
    (should-not (my/ghq-bare-repository-p dir))))

;;; --- 2/3. my/ghq-repo-worktrees ---------------------------------------

(ert-deftest ghq-repo-worktrees/empty-for-bare-repo-with-no-worktrees ()
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (should (null (my/ghq-repo-worktrees bare)))))

(ert-deftest ghq-repo-worktrees/lists-added-worktree-excludes-bare-repo-itself ()
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (let* ((parent (file-truename (make-temp-file "ghq-wt-test-parent-" t)))
           (wt (expand-file-name "wt" parent)))
      (unwind-protect
          (progn
            (test--git bare "worktree" "add" "--detach" wt "main")
            (let ((worktrees (my/ghq-repo-worktrees bare)))
              (should (equal (list (file-truename wt)) (mapcar #'file-truename worktrees)))
              (should-not (member (file-truename bare) (mapcar #'file-truename worktrees)))))
        (delete-directory parent t)))))

;;; --- 4. my/ghq--resolve-base-ref ---------------------------------------

(ert-deftest ghq-resolve-base-ref/falls-through-to-local-head-branch-name ()
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (should (null (my/ghq--git bare "remote" "get-url" "origin")))
    (should (string= "main" (my/ghq--resolve-base-ref bare)))))

;;; --- 5. my/ghq-create-worktree: name, existence, detached HEAD --------

(ert-deftest ghq-create-worktree/creates-detached-worktree-at-expected-name ()
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (let ((target (my/ghq-create-worktree bare)))
      (should (file-directory-p target))
      (should (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]+\\'"
                              (file-name-nondirectory (directory-file-name target))))
      (should (string= "HEAD" (test--git target "rev-parse" "--abbrev-ref" "HEAD"))))))

;;; --- 6. my/ghq-create-worktree: collision-suffix invariant ------------

(ert-deftest ghq-create-worktree/second-call-with-same-timestamp-gets-suffixed-name ()
  "Guards the existing suffix-on-collision loop in `my/ghq-create-worktree'
\(init.org). That loop already ships in the reviewed code, so there is no
separate pre-fix baseline in this repo to run this test against; this test
instead pins the loop's observable invariant directly: two calls forced to
compute the identical base name (via a fixed `format-time-string' stub
installed through `cl-letf') must still produce two distinct worktree
directories, with the second one suffixed \"-2\"."
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _) "20260101T000000")))
      (let* ((first (my/ghq-create-worktree bare))
             (second (my/ghq-create-worktree bare)))
        (should (file-directory-p first))
        (should (file-directory-p second))
        (should-not (string= first second))
        (should (string-suffix-p "-2" second))
        (should-not (string-suffix-p "-2" first))))))

;;; --- 7. my/ghq--link-state / my/ghq-create-worktree: state linking ----

(ert-deftest ghq-create-worktree/links-shared-state-into-new-worktree ()
  (test--with-tmpdir bare
    (test--make-bare-repo-with-main bare)
    (let ((memories-dir (expand-file-name ".state/.serena/memories" bare))
          (claude-dir (expand-file-name ".state/.claude" bare)))
      (make-directory memories-dir t)
      (make-directory claude-dir t)
      (write-region "marker" nil (expand-file-name "marker.txt" memories-dir))
      (write-region "marker" nil (expand-file-name "marker.txt" claude-dir))
      (let ((target (my/ghq-create-worktree bare)))
        (should (file-symlink-p (expand-file-name ".serena/memories" target)))
        (should (file-symlink-p (expand-file-name ".claude" target)))
        (should (string= (file-truename memories-dir)
                          (file-truename (expand-file-name ".serena/memories" target))))
        (should (string= (file-truename claude-dir)
                          (file-truename (expand-file-name ".claude" target))))))))

(ert-deftest ghq-link-state/does-not-shadow-checkout-provided-real-directory ()
  (test--with-tmpdirs (bare target)
    (let ((state-claude (expand-file-name ".state/.claude" bare))
          (checkout-claude (expand-file-name ".claude" target)))
      (make-directory state-claude t)
      (write-region "from-state" nil (expand-file-name "marker.txt" state-claude))
      (make-directory checkout-claude t)
      (write-region "from-checkout" nil (expand-file-name "marker.txt" checkout-claude))
      (my/ghq--link-state bare target)
      (should (file-directory-p checkout-claude))
      (should-not (file-symlink-p checkout-claude))
      (should (string= "from-checkout"
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name "marker.txt" checkout-claude))
                          (buffer-string)))))))

;;; --- 8. my/update-magit-repository-directories --------------------------

(ert-deftest update-magit-repository-directories/dir-dot-zero-alist-preserves-order-and-abbreviation ()
  (test--with-tmpdirs (dir-a dir-b)
    (let* ((roots (list dir-a dir-b))
           (expected (mapcar (lambda (d) (cons (abbreviate-file-name (file-name-as-directory d)) 0))
                              roots)))
      (my/update-magit-repository-directories roots)
      (should (equal expected magit-repository-directories)))))

;;; --- 9. my/update-ghq-project-state: roots computed once --------------

(ert-deftest update-ghq-project-state/computes-roots-once-and-shares-across-both-consumers ()
  (test--with-tmpdirs (dir-a dir-b)
    (let* ((roots (list dir-a dir-b))
           (call-count 0))
      (cl-letf (((symbol-function 'my/ghq-project-roots)
                 (lambda () (setq call-count (1+ call-count)) roots))
                ((symbol-function 'projectile-clear-known-projects) #'ignore)
                ((symbol-function 'projectile-cleanup-known-projects) #'ignore))
        (my/update-ghq-project-state))
      (should (= 1 call-count))
      (should (equal (mapcar (lambda (d) (abbreviate-file-name (file-name-as-directory d))) roots)
                     projectile-known-projects))
      (should (equal (mapcar (lambda (d) (cons (abbreviate-file-name (file-name-as-directory d)) 0)) roots)
                     magit-repository-directories)))))

;;; --- 10/11/12. my/ghq-switch-project ------------------------------------

(ert-deftest ghq-switch-project/non-bare-repo-resolves-directly-with-one-prompt ()
  (test--with-tmpdir nonbare
    (make-directory (expand-file-name ".git" nonbare) t)
    (let ((completing-read-calls 0)
          (switch-target nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq completing-read-calls (1+ completing-read-calls)) nonbare))
                ((symbol-function 'projectile-switch-project-by-name)
                 (lambda (name) (setq switch-target name))))
        (my/ghq-switch-project))
      (should (= 1 completing-read-calls))
      (should (string= nonbare switch-target)))))

(ert-deftest ghq-switch-project/bare-repo-with-zero-worktrees-skips-worktree-prompt ()
  (test--with-tmpdir bare
    (make-directory bare t) ; already exists; no .git entry, so it reads as bare
    (let ((completing-read-calls 0)
          (create-worktree-calls 0)
          (create-worktree-arg nil)
          (switch-target nil)
          (sentinel "the-new-worktree-path"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq completing-read-calls (1+ completing-read-calls)) bare))
                ((symbol-function 'my/ghq-repo-worktrees) (lambda (_repo) nil))
                ((symbol-function 'my/ghq-create-worktree)
                 (lambda (repo &optional _base-ref)
                   (setq create-worktree-calls (1+ create-worktree-calls))
                   (setq create-worktree-arg repo)
                   sentinel))
                ((symbol-function 'projectile-switch-project-by-name)
                 (lambda (name) (setq switch-target name))))
        (my/ghq-switch-project))
      (should (= 1 completing-read-calls))
      (should (= 1 create-worktree-calls))
      (should (string= bare create-worktree-arg))
      (should (string= sentinel switch-target)))))

(ert-deftest ghq-switch-project/bare-repo-with-worktrees-picking-new-worktree-prompts-twice ()
  (test--with-tmpdir bare
    (let ((completing-read-calls 0)
          (create-worktree-calls 0)
          (switch-target nil)
          (sentinel "the-new-worktree-path")
          (existing-worktrees (list "/tmp/existing-wt-1")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest args)
                   (setq completing-read-calls (1+ completing-read-calls))
                   (if (= completing-read-calls 1)
                       bare
                     ;; Second prompt: confirm it was offered "[new worktree]"
                     ;; plus the existing worktrees, then pick the sentinel.
                     (let ((collection (nth 1 args)))
                       (should (member "[new worktree]" collection))
                       (should (member (car existing-worktrees) collection))
                       "[new worktree]"))))
                ((symbol-function 'my/ghq-repo-worktrees) (lambda (_repo) existing-worktrees))
                ((symbol-function 'my/ghq-create-worktree)
                 (lambda (_repo &optional _base-ref)
                   (setq create-worktree-calls (1+ create-worktree-calls))
                   sentinel))
                ((symbol-function 'projectile-switch-project-by-name)
                 (lambda (name) (setq switch-target name))))
        (my/ghq-switch-project))
      (should (= 2 completing-read-calls))
      (should (= 1 create-worktree-calls))
      (should (string= sentinel switch-target)))))

(ert-deftest ghq-switch-project/refreshes-project-state-before-first-prompt ()
  "Guards the fix that made `my/ghq-switch-project' call
`my/update-ghq-project-state' explicitly as its first action. Before the
fix, the refresh relied on a `:before' advice on `projectile-switch-project'
that could never fire (an `:override' advice registered afterward on the
same symbol discarded it, and `my/ghq-switch-project' never called
`projectile-switch-project' anyway), so `projectile-known-projects' and
`magit-repository-directories' silently went stale after Emacs startup.
This test proves the refresh happens, and specifically that it happens
before the repository picker is shown, by having both the refresh spy and
the `completing-read' stub push markers onto a shared ordered list and
asserting the refresh marker precedes the first `completing-read' marker."
  (test--with-tmpdir nonbare
    (make-directory (expand-file-name ".git" nonbare) t)
    (let ((call-order nil)
          (refresh-calls 0))
      (cl-letf (((symbol-function 'my/update-ghq-project-state)
                 (lambda ()
                   (setq refresh-calls (1+ refresh-calls))
                   (push 'refresh call-order)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (push 'completing-read call-order) nonbare))
                ((symbol-function 'projectile-switch-project-by-name) #'ignore))
        (my/ghq-switch-project))
      (setq call-order (nreverse call-order))
      (should (= 1 refresh-calls))
      (should (equal '(refresh completing-read) call-order)))))

(provide 'ghq-worktree-project-picker-test)

;;; ghq-worktree-project-picker-test.el ends here
