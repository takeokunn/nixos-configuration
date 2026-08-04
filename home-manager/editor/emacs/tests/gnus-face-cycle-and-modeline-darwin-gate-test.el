;;; gnus-face-cycle-and-modeline-darwin-gate-test.el --- ERT regression tests -*- lexical-binding: t; -*-

;; This repo has no pre-existing elisp test framework (no ert-deftest,
;; buttercup, Eldev, or tests/ dir was found anywhere in the tree at the
;; time this file was added). This is a minimal, standalone ERT suite
;; invoked directly via `emacs --batch'; see the command at the bottom of
;; this file for exact invocation.
;;
;; It guards two fixes made in
;; home-manager/editor/emacs/elisp/init.org (the "Doom" / doom-themes and
;; doom-modeline sections):
;;
;; 1. The `gnus-group-news-low-empty' face-inheritance-cycle fix.
;; 2. Darwin-gating of the `gnus' doom-modeline segment (mirrors the
;;    existing `mu4e' gating).
;;
;; Prerequisite: run via the Nix/home-manager-managed `emacs' on PATH
;; (the one built by this repo), since it needs the real `doom-themes'
;; and (built-in) `gnus-group' libraries on its load-path. A plain
;; system Emacs without those packages installed will fail to load this
;; suite with `require' errors -- that is expected, not a bug in the
;; test.

(require 'ert)

;;; --- Part 1: gnus-group-news-low-empty face-inheritance-cycle fix ---
;;
;; Background (verified interactively before writing this test):
;;
;; - Emacs 31's built-in gnus-group.el defines
;;     (defface gnus-group-news-low '((t (:inherit gnus-group-news-low-empty ...))))
;; - doom-themes' doom-dracula theme (doom-themes-base.el) defines
;;     (gnus-group-news-low-empty :inherit 'gnus-group-news-low ...)
;;   i.e. the two faces mutually inherit from each other once both specs
;;   are active: a cycle.
;; - gnus-group.el loads lazily, often long after doom-dracula in a
;;   running daemon. The moment it does, Emacs resolves the cycle
;;   *synchronously inside `custom-declare-face'*, signalling
;;   (error "Face inheritance results in inheritance cycle" ...)
;;   *before* `(provide 'gnus-group)' is ever reached.
;;
;; This has a critical consequence that this test specifically guards
;; against: a naive `(with-eval-after-load 'gnus-group ...)' guard that
;; pokes the face's attributes NEVER RUNS, because the error aborts the
;; `require' before the post-load hook fires. This was confirmed by
;; direct reproduction:
;;
;;   (require 'doom-themes)
;;   (load-theme 'doom-dracula t)
;;   (require 'gnus-group)   ; => signals the cycle error right here;
;;                           ;    (featurep 'gnus-group) is nil afterwards.
;;
;; The shipped fix instead re-registers a corrected (non-cyclic) face
;; spec under the *already-active theme* (`custom-theme-set-faces' +
;; `enable-theme') immediately after `load-theme', so the corrected spec
;; is already present by the time gnus-group.el ever defines the face --
;; regardless of how much later that happens.

(defun test--apply-gnus-face-cycle-fix ()
  "Reproduce exactly what the after-init-hook lambda in init.org does
for the doom-dracula / gnus-group-news-low-empty cycle, assuming
doom-themes has already been required and `doom-dracula' already loaded."
  (custom-theme-set-faces
   'doom-dracula '(gnus-group-news-low-empty ((t :inherit nil))))
  (enable-theme 'doom-dracula))

(defun test--assert-no-face-cycle ()
  "Assert the fix is durably in effect: no cycle when Emacs is asked to
fully resolve `gnus-group-news-low's inheritance chain, and no cycle
when face specs are recalculated for a (frame) -- the same recalculation
a newly created corfu popup frame triggers, which is what the original
bug report described as the crash trigger."
  (should (facep 'gnus-group-news-low-empty))
  (should (null (face-attribute 'gnus-group-news-low-empty :inherit nil t)))
  ;; Smoke check only -- not the crash-reproducing assertion. This calls
  ;; the same primitive (`face-spec-recalc') that per-frame face-spec
  ;; recalculation on new frame creation would invoke (batch mode has no
  ;; real display, so a literal make-frame is not usable here), but
  ;; Emacs's actual new-frame initializer (`face-set-after-frame-default'
  ;; in faces.el) wraps that call in `condition-case' and swallows
  ;; errors, so a bare call here would never actually reproduce the
  ;; original crash the way it might appear to.
  (should (progn (face-spec-recalc 'gnus-group-news-low-empty (selected-frame))
                 (face-spec-recalc 'gnus-group-news-low (selected-frame))
                 t))
  ;; This is the load-bearing assertion: `:inherit t' forces full
  ;; resolution of the inheritance chain -- the same kind of resolution
  ;; the original bug report says corfu forces -- and, unlike
  ;; `face-spec-recalc' above, errors here are NOT swallowed by any
  ;; `condition-case', so this is the real proxy for the crash. Any
  ;; signal here (e.g. the cycle error) propagates out of `should' and
  ;; fails the test; reaching the `should' with a non-nil probe value is
  ;; the passing case.
  (should (progn (face-attribute 'gnus-group-news-low :inherit t) t)))

(ert-deftest gnus-face-cycle-fix/handles-both-load-orders ()
  "Covers both load orders in one explicitly-sequenced test, rather than
relying on cross-test execution order: ERT's batch runner does not run
tests in file/definition order (observed: it sorts by test name), and
`require'/`load-theme' effects are process-global and cannot be cleanly
undone between separate `ert-deftest' forms -- so a same-process,
explicitly-ordered sequence is the only reliable way to exercise both
orders here.

Phase 1 (`ert-info' \"fix applied before gnus-group loads\"): matches the
real production order -- the theme (and fix) load once at startup via
after-init-hook; gnus-group.el loads much later (lazily, e.g. days into
a daemon session). This is the order that crashes without the fix
(verified manually outside this suite: `require \\='gnus-group' signals
the cycle error and `(provide \\='gnus-group)' is never reached).

Phase 2 (`ert-info' \"fix re-applied after gnus-group already loaded\"):
robustness/idempotency check for the reverse order, using the
already-loaded gnus-group from phase 1."
  (ert-info ("fix applied before gnus-group loads (production order)")
    (require 'doom-themes)
    (load-theme 'doom-dracula t)
    (test--apply-gnus-face-cycle-fix)
    ;; With the fix in place, this must NOT signal.
    (should (require 'gnus-group nil t))
    (test--assert-no-face-cycle))
  (ert-info ("fix re-applied after gnus-group already loaded (idempotency)")
    (should (featurep 'gnus-group))
    (test--apply-gnus-face-cycle-fix)
    (test--assert-no-face-cycle)))

;;; --- Part 2: doom-modeline `gnus' segment Darwin-gating ---
;;
;; mu4e/mail was already excluded on Darwin (arm64 `mailutils' build
;; failure, see commit b3ebd7e7) via:
;;   (when (not (eq system-type 'darwin)) '(mu4e))
;; inserted into the `append' list building `main-segments'. The `gnus'
;; segment must be gated the same way, since it pulls gnus-group.el into
;; a running Darwin daemon (see Part 1) and Linux behavior must not
;; change.
;;
;; The full modeline block is too heavy to load in a unit test (it's
;; wrapped in `with-eval-after-load 'doom-modeline' and calls
;; `doom-modeline-def-modeline'/`doom-modeline-def-segment', which pulls
;; in doom-modeline and its own dependency chain). Per the task's own
;; guidance this replicates just the list-construction expression as a
;; small parameterized helper, kept textually close to
;; home-manager/editor/emacs/elisp/init.org's `main-segments' `let' form
;; (~line 4339 at the time of writing) so it stays easy to keep in sync.

(defun test--build-main-segments (system-type)
  "Mirror of the `main-segments' construction in init.org, parameterized
on SYSTEM-TYPE instead of reading the global `system-type'."
  (append '(compilation objed-state misc-info project-name persp-name battery grip irc)
          (when (not (eq system-type 'darwin))
            '(mu4e))
          (when (not (eq system-type 'darwin))
            '(gnus))
          '(github debug repl lsp my/nskk minor-modes input-method indent-info
                    buffer-encoding major-mode process vcs check time)))

(ert-deftest doom-modeline-darwin-gate/gnus-excluded-on-darwin ()
  (let ((segments (test--build-main-segments 'darwin)))
    (should-not (memq 'gnus segments))
    (should-not (memq 'mu4e segments))))

(ert-deftest doom-modeline-darwin-gate/gnus-included-on-linux ()
  (let ((segments (test--build-main-segments 'gnu/linux)))
    (should (memq 'gnus segments))
    (should (memq 'mu4e segments))))

(provide 'gnus-face-cycle-and-modeline-darwin-gate-test)

;; Invocation (run from the repo root; requires the repo's Nix/
;; home-manager-built `emacs' on PATH, since it needs the real
;; `doom-themes' and built-in `gnus-group' libraries):
;;
;;   emacs --batch -l home-manager/editor/emacs/tests/gnus-face-cycle-and-modeline-darwin-gate-test.el -f ert-run-tests-batch-and-exit

;;; gnus-face-cycle-and-modeline-darwin-gate-test.el ends here
