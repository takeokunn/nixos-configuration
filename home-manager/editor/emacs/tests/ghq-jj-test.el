;;; ghq-jj-test.el --- jj worktree helper tests for init.org -*- lexical-binding: t; -*-

;; init.org creates worktrees as jj workspaces of ghq's bare repositories
;; (`my/ghq-create-worktree' and its helpers), picks majutsu or magit by VCS
;; kind (`my/magit-status'), and builds GitHub URLs from jj state
;; (`my/copy-projectile-github-url').  The defuns are read out of init.org
;; rather than restated here, so these tests fail if the code drifts.
;;
;; The tests run the real jj and git binaries against throwaway repositories
;; cloned from a local origin.  Nothing needs the network, a user-level jj or
;; git configuration, or gh: gh is replaced by a stub script, and the
;; packages (projectile, magit, majutsu) are stubbed where a defun calls them.
;;
;; Run from the repository root:
;;
;;   emacs --batch -l home-manager/editor/emacs/tests/ghq-jj-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst ghq-jj-test--init-org
  (expand-file-name "../elisp/init.org"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst ghq-jj-test--defuns
  '("my/ghq-bare-repository-p" "my/vcs-root" "my/jj-call" "my/jj-output"
    "my/jj-output-or-error" "my/magit-status"
    "my/ghq--git" "my/ghq-repo-worktrees"
    "my/ghq--jj-init-bare" "my/ghq--jj-run" "my/ghq--jj-fetch"
    "my/ghq--jj-resolves-p" "my/ghq--resolve-base-ref" "my/ghq--link-state"
    "my/ghq-create-worktree" "my/ghq--current-repo-path"
    "my/remote-url-to-https" "my/jj-origin-url" "my/copy-projectile-github-url"
    "my/jj-checkout-pr")
  "Defuns evaluated from init.org.")

(defun ghq-jj-test--load-defuns ()
  "Evaluate every defun in `ghq-jj-test--defuns' from init.org."
  (with-temp-buffer
    (insert-file-contents ghq-jj-test--init-org)
    (dolist (name ghq-jj-test--defuns)
      (goto-char (point-min))
      (unless (search-forward (format "(defun %s " name) nil t)
        (error "%s is not defined in %s" name ghq-jj-test--init-org))
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(ghq-jj-test--load-defuns)

(defconst ghq-jj-test--real-jj (executable-find "jj"))

(defmacro ghq-jj-test--with-sandbox (&rest body)
  "Run BODY in a fresh directory bound to `root' with a clean jj/git identity."
  (declare (indent 0))
  `(let* ((root (file-truename (make-temp-file "ghq-jj-test-" t)))
          (home (expand-file-name "home" root))
          (process-environment
           (append (list (concat "HOME=" home)
                         "JJ_USER=Test" "JJ_EMAIL=test@example.com"
                         "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                         "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=test@example.com"
                         "GIT_CONFIG_NOSYSTEM=1" "GIT_CONFIG_GLOBAL=/dev/null"
                         (concat "JJ_CONFIG=" (expand-file-name "jj-config.toml" root)))
                   process-environment)))
     (make-directory home t)
     (write-region "" nil (expand-file-name "jj-config.toml" root) nil 'silent)
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ghq-jj-test--run (dir &rest cmd)
  "Run CMD in DIR, return trimmed stdout, and fail the test on nonzero exit."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (exit (apply #'call-process (car cmd) nil t nil (cdr cmd))))
      (unless (zerop exit)
        (error "%S failed (%s): %s" cmd exit (buffer-string)))
      (string-trim (buffer-string)))))

(defun ghq-jj-test--make-origin (root branches)
  "Create a non-bare origin under ROOT whose BRANCHES each hold a distinct commit.
The first of BRANCHES is HEAD.  Return the origin path."
  (let ((origin (expand-file-name "origin" root))
        (tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))
    (make-directory origin t)
    (ghq-jj-test--run origin "git" "init" "-q" "-b" (car branches))
    (dolist (b branches)
      (ghq-jj-test--run
       origin "git" "update-ref" (concat "refs/heads/" b)
       (ghq-jj-test--run origin "git" "commit-tree" tree "-m" b)))
    origin))

(defun ghq-jj-test--origin-sha (origin branch)
  "Return the full commit id of BRANCH in ORIGIN."
  (ghq-jj-test--run origin "git" "rev-parse" (concat "refs/heads/" branch)))

(defun ghq-jj-test--make-bare (root branches)
  "Return a bare clone of a new origin with BRANCHES, as ghq lays it out.
A `git clone --bare' repository has no remote.origin.fetch refspec."
  (let* ((origin (ghq-jj-test--make-origin root branches))
         (bare (expand-file-name "repo.git" root)))
    (ghq-jj-test--run root "git" "clone" "-q" "--bare" origin bare)
    bare))

(defun ghq-jj-test--workspace-names (repo)
  "Return the jj workspace names of REPO."
  (split-string (ghq-jj-test--run repo "jj" "-R" repo "--ignore-working-copy"
                                  "workspace" "list" "-T" "name ++ \"\\n\"")
                "\n" t))

(defun ghq-jj-test--git-worktrees (repo)
  "Return the porcelain `git worktree list' output of REPO."
  (ghq-jj-test--run repo "git" "-C" repo "worktree" "list" "--porcelain"))

(defun ghq-jj-test--parent-sha (workspace)
  "Return the short commit id of the parent of WORKSPACE's working copy."
  (ghq-jj-test--run workspace "jj" "--ignore-working-copy" "log" "--no-graph"
                    "-r" "@-" "-T" "commit_id.short(8)"))

(defun ghq-jj-test--make-stub-bin (root name script)
  "Write executable NAME with shell SCRIPT under ROOT/stubbin; return that dir."
  (let ((dir (expand-file-name "stubbin" root))
        (file nil))
    (make-directory dir t)
    (setq file (expand-file-name name dir))
    (with-temp-file file (insert "#!/bin/sh\n" script "\n"))
    (set-file-modes file #o755)
    dir))

;;; C8: VCS kind detection

(ert-deftest ghq-jj/vcs-root-kinds ()
  (ghq-jj-test--with-sandbox
    (let ((jj (expand-file-name "jj/sub/dir/" root))
          (git (expand-file-name "git/sub/" root))
          (colocated (expand-file-name "colo/sub/" root))
          (nested (expand-file-name "outer/inner/sub/" root))
          (none (expand-file-name "none/" root)))
      (dolist (d (list jj git colocated nested none)) (make-directory d t))
      (make-directory (expand-file-name "jj/.jj" root))
      (make-directory (expand-file-name "git/.git" root))
      (make-directory (expand-file-name "colo/.jj" root))
      (make-directory (expand-file-name "colo/.git" root))
      (make-directory (expand-file-name "outer/.jj" root))
      ;; a worktree's .git is a file, not a directory
      (write-region "gitdir: x" nil (expand-file-name "outer/inner/.git" root))
      (should (equal (my/vcs-root jj) (cons 'jj (expand-file-name "jj/" root))))
      (should (equal (my/vcs-root git) (cons 'git (expand-file-name "git/" root))))
      (should (equal (my/vcs-root colocated) (cons 'jj (expand-file-name "colo/" root))))
      (should (equal (my/vcs-root nested) (cons 'git (expand-file-name "outer/inner/" root))))
      (should-not (my/vcs-root none)))))

;;; C2: jj init of a bare repository

(ert-deftest ghq-jj/init-bare-creates-store ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (before (directory-files bare)))
      (should (my/ghq-bare-repository-p bare))
      (my/ghq--jj-init-bare bare)
      (should (file-directory-p (expand-file-name ".jj" bare)))
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name ".jj/repo/store/git_target" bare))
                       (buffer-string))
                     "../../.."))
      (should (equal (ghq-jj-test--workspace-names bare) '("default")))
      (should (equal (sort (copy-sequence (directory-files bare))
                           #'string<)
                     (sort (cons ".jj" before) #'string<))))))

(ert-deftest ghq-jj/init-bare-rolls-back-on-failure ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (before (sort (copy-sequence (directory-files bare)) #'string<))
           (stub-dir (ghq-jj-test--make-stub-bin
                      root "jj"
                      (format "for a in \"$@\"; do [ \"$a\" = sparse ] && exit 1; done\nexec %s \"$@\""
                              ghq-jj-test--real-jj))))
      (let ((exec-path (cons stub-dir exec-path))
            (process-environment (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                                       process-environment)))
        (should-error (my/ghq--jj-init-bare bare)))
      (should-not (file-exists-p (expand-file-name ".jj" bare)))
      (should (equal (sort (copy-sequence (directory-files bare)) #'string<) before))
      ;; control: without the stub the same repository initialises
      (my/ghq--jj-init-bare bare)
      (should (file-directory-p (expand-file-name ".jj" bare))))))

(ert-deftest ghq-jj/init-bare-rejects-non-bare ()
  (ghq-jj-test--with-sandbox
    (let ((origin (ghq-jj-test--make-origin root '("main"))))
      (should-error (my/ghq--jj-init-bare origin))
      (should-not (file-exists-p (expand-file-name ".jj" origin))))))

;;; C4: default ref resolution

(defun ghq-jj-test--resolve (root branches &optional head)
  "Initialise a bare clone with BRANCHES and return its resolved base ref.
HEAD, when non-nil, replaces the branch the bare repository's HEAD names."
  (let ((bare (ghq-jj-test--make-bare root branches)))
    (when head
      (ghq-jj-test--run bare "git" "symbolic-ref" "HEAD" (concat "refs/heads/" head)))
    (my/ghq--jj-init-bare bare)
    (my/ghq--resolve-base-ref bare)))

(ert-deftest ghq-jj/resolve-prefers-main ()
  (ghq-jj-test--with-sandbox
    (should (equal (ghq-jj-test--resolve root '("trunk" "master" "main")) "main@origin"))))

(ert-deftest ghq-jj/resolve-master-only ()
  (ghq-jj-test--with-sandbox
    (should (equal (ghq-jj-test--resolve root '("master" "feature/x")) "master@origin"))))

(ert-deftest ghq-jj/resolve-trunk-after-main-and-master ()
  (ghq-jj-test--with-sandbox
    (should (equal (ghq-jj-test--resolve root '("trunk" "dev")) "trunk@origin"))))

(ert-deftest ghq-jj/resolve-falls-back-to-head-file ()
  (ghq-jj-test--with-sandbox
    (should (equal (ghq-jj-test--resolve root '("develop" "other")) "develop"))))

(ert-deftest ghq-jj/resolve-errors-when-nothing-resolves ()
  (ghq-jj-test--with-sandbox
    (should-error (ghq-jj-test--resolve root '("develop") "absent"))))

;;; C5: workspace creation

(ert-deftest ghq-jj/create-worktree-makes-workspace-not-git-worktree ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main" "feature/x")))
           (git-before (ghq-jj-test--git-worktrees bare))
           (main-sha (ghq-jj-test--origin-sha (expand-file-name "origin" root) "main"))
           (path (my/ghq-create-worktree bare)))
      (should (file-directory-p (expand-file-name ".jj" bare)))
      (should (string-prefix-p (file-name-as-directory (expand-file-name ".worktrees" bare)) path))
      (should (file-directory-p (expand-file-name ".jj" path)))
      (should (member (file-name-nondirectory path) (ghq-jj-test--workspace-names bare)))
      (should (equal (ghq-jj-test--git-worktrees bare) git-before))
      (should (equal (ghq-jj-test--parent-sha path) (substring main-sha 0 8)))
      (should (string-suffix-p (substring main-sha 0 8) path)))))

(ert-deftest ghq-jj/create-worktree-branch-naming ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main" "feature/x")))
           (origin (expand-file-name "origin" root))
           (default (my/ghq-create-worktree bare nil 'branch))
           (feature (my/ghq-create-worktree bare "feature/x@origin" 'branch)))
      (should (string-match-p "/[0-9T]+-main\\'" default))
      (should (string-match-p "/[0-9T]+-feature-x\\'" feature))
      (should (equal (ghq-jj-test--parent-sha feature)
                     (substring (ghq-jj-test--origin-sha origin "feature/x") 0 8))))))

(ert-deftest ghq-jj/create-worktree-rejects-multi-commit-revset ()
  (ghq-jj-test--with-sandbox
    (let ((bare (ghq-jj-test--make-bare root '("main" "dev"))))
      (should-error (my/ghq-create-worktree bare "main@origin | dev@origin"))
      (should-error (my/ghq-create-worktree bare "no-such-ref"))
      (should-not (file-directory-p (expand-file-name ".worktrees" bare))))))

(ert-deftest ghq-jj/create-worktree-rejects-non-bare-without-store ()
  (ghq-jj-test--with-sandbox
    (let ((origin (ghq-jj-test--make-origin root '("main"))))
      (should-error (my/ghq-create-worktree origin "main"))
      (should-not (file-exists-p (expand-file-name ".worktrees" origin))))))

(ert-deftest ghq-jj/repo-worktrees-lists-workspace-and-legacy-git-worktree ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (ws (my/ghq-create-worktree bare))
           (legacy (expand-file-name ".worktrees/legacy" bare)))
      (ghq-jj-test--run bare "git" "-C" bare "worktree" "add" "--detach" legacy "main")
      (let ((listed (my/ghq-repo-worktrees bare)))
        (should (member ws (mapcar #'file-truename listed)))
        (should (member (file-truename legacy) (mapcar #'file-truename listed)))
        (should-not (member (file-truename bare) (mapcar #'file-truename listed)))))))

;;; C6: current repository path

(ert-deftest ghq-jj/current-repo-path-from-workspace-and-legacy-worktree ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (ws (my/ghq-create-worktree bare))
           (legacy (expand-file-name ".worktrees/legacy" bare)))
      (ghq-jj-test--run bare "git" "-C" bare "worktree" "add" "--detach" legacy "main")
      (let ((default-directory (file-name-as-directory ws)))
        (should (equal (file-truename (my/ghq--current-repo-path)) (file-truename bare))))
      (let ((default-directory (file-name-as-directory legacy)))
        (should (equal (file-truename (my/ghq--current-repo-path)) (file-truename bare)))))))

(ert-deftest ghq-jj/current-repo-path-outside-repo-errors ()
  (ghq-jj-test--with-sandbox
    (let ((default-directory (file-name-as-directory root)))
      (should-error (my/ghq--current-repo-path)))))

;;; C9: GitHub URL from jj state

(ert-deftest ghq-jj/copy-github-url-uses-remote-commit-in-jj ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (origin (expand-file-name "origin" root))
           (ws (my/ghq-create-worktree bare))
           (main-sha (ghq-jj-test--origin-sha origin "main"))
           copied)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "src/a.txt" ws))
        (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
                  ((symbol-function 'projectile-project-root)
                   (lambda () (file-name-as-directory ws)))
                  ((symbol-function 'kill-new) (lambda (s) (setq copied s))))
          (my/copy-projectile-github-url)))
      (should (equal copied (format "%s/blob/%s/src/a.txt#L1" origin main-sha))))))

;;; C10: C-x g dispatch

(ert-deftest ghq-jj/magit-status-dispatches-by-vcs-kind ()
  (ghq-jj-test--with-sandbox
    (let ((jj (file-name-as-directory (expand-file-name "jj" root)))
          (git (file-name-as-directory (expand-file-name "git" root)))
          calls)
      (make-directory (expand-file-name ".jj" jj) t)
      (make-directory (expand-file-name ".git" git) t)
      (cl-letf (((symbol-function 'majutsu-log) (lambda (dir) (push (list 'majutsu dir) calls)))
                ((symbol-function 'magit-status) (lambda () (push (list 'magit default-directory) calls))))
        (let ((default-directory jj)) (my/magit-status))
        (let ((default-directory git)) (my/magit-status)))
      (should (equal (nreverse calls) (list (list 'majutsu jj) (list 'magit git)))))))

;;; jj stub: records every invocation, optionally intercepting some

(defun ghq-jj-test--call-with-jj-stub (root intercept fn)
  "Call FN with a jj stub first on PATH.
The stub appends its arguments to ROOT/jj.log, runs the shell snippet
INTERCEPT (which may exit), then execs the real jj."
  (let* ((log (expand-file-name "jj.log" root))
         (stub-dir (ghq-jj-test--make-stub-bin
                    root "jj"
                    (format "printf '%%s\\n' \"$*\" >> %s\n%s\nexec %s \"$@\""
                            (shell-quote-argument log) intercept ghq-jj-test--real-jj)))
         (exec-path (cons stub-dir exec-path))
         (process-environment (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                                    process-environment)))
    (funcall fn)))

(defun ghq-jj-test--jj-log (root)
  "Return the stub's recorded invocations under ROOT as a string."
  (let ((file (expand-file-name "jj.log" root)))
    (if (file-exists-p file)
        (with-temp-buffer (insert-file-contents file) (buffer-string))
      "")))

(defun ghq-jj-test--error-message (thunk)
  "Call THUNK, which must signal an error, and return the error message."
  (condition-case err (progn (funcall thunk) (ert-fail "no error signalled"))
    (ert-test-failed (signal (car err) (cdr err)))
    (error (error-message-string err))))

;;; Fixes: full commit id, rollback, bare-p, stderr

(ert-deftest ghq-jj/create-worktree-passes-full-commit-id-to-workspace-add ()
  (ghq-jj-test--with-sandbox
    (let ((bare (ghq-jj-test--make-bare root '("main"))))
      (ghq-jj-test--call-with-jj-stub
       root "" (lambda () (my/ghq-create-worktree bare)))
      (let ((line (seq-find (lambda (l) (string-match-p "workspace add" l))
                            (split-string (ghq-jj-test--jj-log root) "\n" t))))
        (should line)
        (should (string-match " -r \\([0-9a-f]+\\) " line))
        (should (= (length (match-string 1 line)) 40))))))

(ert-deftest ghq-jj/init-bare-late-failure-removes-store ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (msg (ghq-jj-test--call-with-jj-stub
                 root "case \" $* \" in *\" workspace list \"*) exit 1;; esac"
                 (lambda ()
                   (ghq-jj-test--error-message (lambda () (my/ghq--jj-init-bare bare)))))))
      (should (string-match-p "workspace list failed" msg))
      (should-not (file-exists-p (expand-file-name ".jj" bare)))
      (should-not (seq-filter (lambda (f) (string-prefix-p ".jj-init." f)) (directory-files bare))))))

(ert-deftest ghq-jj/init-bare-concurrent-store-is-kept ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (keep (expand-file-name ".jj/keep" bare))
           (msg (ghq-jj-test--call-with-jj-stub
                 root (format "case \" $* \" in *\" sparse \"*) mkdir -p %s;; esac"
                              (shell-quote-argument keep))
                 (lambda ()
                   (ghq-jj-test--error-message (lambda () (my/ghq--jj-init-bare bare)))))))
      (should (string-match-p "concurrent jj init" msg))
      (should (file-directory-p keep))
      (should-not (seq-filter (lambda (f) (string-prefix-p ".jj-init." f)) (directory-files bare))))))

(ert-deftest ghq-jj/bare-p-treats-dangling-git-symlink-as-non-bare ()
  (ghq-jj-test--with-sandbox
    (let ((empty (expand-file-name "empty" root))
          (real (expand-file-name "real" root))
          (dangling (expand-file-name "dangling" root)))
      (dolist (d (list empty real dangling)) (make-directory d t))
      (make-directory (expand-file-name ".git" real))
      (make-symbolic-link "/nonexistent-ghq-jj-test-target" (expand-file-name ".git" dangling))
      (should (my/ghq-bare-repository-p empty))
      (should-not (my/ghq-bare-repository-p real))
      (should-not (my/ghq-bare-repository-p dangling)))))

(ert-deftest ghq-jj/import-failure-message-carries-jj-stderr ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (msg (ghq-jj-test--call-with-jj-stub
                 root "case \" $* \" in *\" git import \"*) echo boom-stderr >&2; exit 1;; esac"
                 (lambda ()
                   (ghq-jj-test--error-message (lambda () (my/ghq-create-worktree bare "main")))))))
      (should (string-match-p "boom-stderr" msg)))))

;;; C9 additions

(ert-deftest ghq-jj/copy-github-url-skips-local-only-commit ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           (origin (expand-file-name "origin" root))
           (ws (my/ghq-create-worktree bare))
           (main-sha (ghq-jj-test--origin-sha origin "main"))
           copied)
      (ghq-jj-test--run ws "jj" "commit" "-m" "local only")
      (should-not (equal (ghq-jj-test--run ws "jj" "--ignore-working-copy" "log" "--no-graph"
                                           "-r" "@-" "-T" "commit_id")
                         main-sha))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "a.txt" ws))
        (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
                  ((symbol-function 'projectile-project-root)
                   (lambda () (file-name-as-directory ws)))
                  ((symbol-function 'kill-new) (lambda (s) (setq copied s))))
          (my/copy-projectile-github-url)))
      (should (equal copied (format "%s/blob/%s/a.txt#L1" origin main-sha))))))

(ert-deftest ghq-jj/copy-github-url-errors-without-remote-ancestor ()
  (ghq-jj-test--with-sandbox
    (let* ((bare (ghq-jj-test--make-bare root '("main")))
           ;; an explicit base ref skips the fetch, so no remote bookmark exists
           (ws (my/ghq-create-worktree bare "main"))
           copied)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "a.txt" ws))
        (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
                  ((symbol-function 'projectile-project-root)
                   (lambda () (file-name-as-directory ws)))
                  ((symbol-function 'kill-new) (lambda (s) (setq copied s))))
          (should (string-match-p "No commit reachable"
                                  (ghq-jj-test--error-message #'my/copy-projectile-github-url)))))
      (should-not copied))))

(ert-deftest ghq-jj/remote-url-to-https-normalises-and-strips-credentials ()
  (should (equal (my/remote-url-to-https "ssh://git@host.example/o/r") "https://host.example/o/r"))
  (should (equal (my/remote-url-to-https "git@host.example:o/r.git") "https://host.example/o/r"))
  (should (equal (my/remote-url-to-https "https://user:tok@host.example/o/r.git") "https://host.example/o/r"))
  (should (equal (my/remote-url-to-https "https://tok@host.example/o/r") "https://host.example/o/r"))
  (should (equal (my/remote-url-to-https "https://host.example/o/r") "https://host.example/o/r")))

;;; C11: PR checkout

(defun ghq-jj-test--pr-run (root head cross-repo)
  "Run `my/jj-checkout-pr' with gh stubbed to report HEAD and CROSS-REPO.
Return a plist with :error (the signalled condition or nil), :ws, :origin,
:log (jj invocations made by the command) and :at-before / :at-after."
  (let* ((bare (ghq-jj-test--make-bare root '("main" "feat")))
         (origin (expand-file-name "origin" root))
         (ws (my/ghq-create-worktree bare))
         (gh-dir (ghq-jj-test--make-stub-bin root "gh" "printf '%s' \"$GH_STUB_JSON\""))
         (exec-path (cons gh-dir exec-path))
         (process-environment
          (append (list (format "GH_STUB_JSON={\"headRefName\":\"%s\",\"isCrossRepository\":%s}"
                                head (if cross-repo "true" "false")))
                  process-environment))
         (default-directory (file-name-as-directory ws))
         (at (lambda () (ghq-jj-test--run ws "jj" "log" "--no-graph" "-r" "@" "-T" "change_id")))
         (before (funcall at))
         (log-before (length (ghq-jj-test--jj-log root)))
         err)
    (ghq-jj-test--call-with-jj-stub
     root "" (lambda () (condition-case e (my/jj-checkout-pr 7) (error (setq err e)))))
    (list :error err :ws ws :origin origin
          :log (substring (ghq-jj-test--jj-log root) log-before)
          :at-before before :at-after (funcall at))))

(ert-deftest ghq-jj/checkout-pr-same-repo-creates-change-on-head-branch ()
  (ghq-jj-test--with-sandbox
    (let ((r (ghq-jj-test--pr-run root "feat" nil)))
      (should-not (plist-get r :error))
      (should (equal (ghq-jj-test--parent-sha (plist-get r :ws))
                     (substring (ghq-jj-test--origin-sha (plist-get r :origin) "feat") 0 8)))
      (should (string-match-p "git fetch --remote origin --branch exact:feat" (plist-get r :log))))))

(ert-deftest ghq-jj/checkout-pr-refuses-cross-repository ()
  (ghq-jj-test--with-sandbox
    (let ((r (ghq-jj-test--pr-run root "feat" t)))
      (should (eq (car (plist-get r :error)) 'user-error))
      (should (equal (plist-get r :at-before) (plist-get r :at-after)))
      (should-not (string-match-p "git fetch" (plist-get r :log))))))

(ert-deftest ghq-jj/checkout-pr-rejects-invalid-head-names ()
  (dolist (head '("-evil" "a b" "x;y" "f*" "a$b"))
    (ghq-jj-test--with-sandbox
      (let ((r (ghq-jj-test--pr-run root head nil)))
        (should (eq (car (plist-get r :error)) 'user-error))
        (should (equal (plist-get r :at-before) (plist-get r :at-after)))
        (should-not (string-match-p "git fetch" (plist-get r :log)))
        (should-not (string-match-p " new " (plist-get r :log)))))))

(provide 'ghq-jj-test)

;;; ghq-jj-test.el ends here
