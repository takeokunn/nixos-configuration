---
name: emacs-lisp-ecosystem
description: Use when editing the Emacs configuration in the nixos-configuration repository (home-manager/editor/emacs/elisp/init.org or early-init.org), adding an Emacs package to it, or checking an Emacs Lisp change there. Covers the Nix-side org-babel tangler and how it differs from org-babel-tangle, Nix-provided packages and the silent autoload-if-found no-op, byte-compile, paren, and ERT checks on a scratch tangle, and verifying without restarting, instrumenting, or evaluating in the user's running Emacs daemon.
metadata:
  version: "1.0.0"
---

Traps in this repository's Emacs configuration, where a wrong edit or a wrong check passes without an
error. Paths are relative to the nixos-configuration repository root. Commands use bash syntax.

## How init.el is produced

- `home-manager/editor/emacs/default.nix` builds `~/.emacs.d/init.el` and `early-init.el` with
  `org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; }` from the `emacs-twist/org-babel` flake
  input. Emacs never tangles these files; the tangler is a Nix regex pass.
- That tangler takes every `#+begin_src emacs-lisp` block whose `:tangle` is absent or exactly `yes`. A
  block with no `:tangle` is tangled; Emacs' own `org-babel-tangle` defaults to `:tangle no` and would drop
  it. init.org relies on this: several bare blocks (for example `my/open-projectile-github-url`) are live
  code. Write `:tangle no` to disable a block, and do not use `org-babel-tangle` to reproduce init.el.
- It ignores every other header argument and does not expand noweb references. `elisp` blocks are not
  tangled; only `emacs-lisp` matches. `COMMENT` and `ARCHIVE` subtrees are not excluded.
- Block lines are copied verbatim, indentation included, and concatenated in file order, so heading order
  is load order and compile order. A form may call a defun from an earlier heading directly; a reference to
  a later heading needs deferral (`with-eval-after-load`, a hook, or a function body that runs later).
- The `lexical-binding` cookie must sit at column 0 of the first tangled line. An indented cookie is
  ignored by `load` (dynamic binding) yet honoured by `byte-compile-file`, so a compile check can pass
  while the loaded file runs dynamically. Check with `head -1 <tangled init.el> | od -c | head -1`.
- Each heading carries a `:PROPERTIES:` drawer with a unique `:CUSTOM_ID:`. Keep that for new headings.
- A push to main also exports init.org and early-init.org to HTML for GitHub Pages
  (`deploy/flake.nix`, `.github/workflows/main.yml`): that job runs `org-publish` over both files, and their
  prose is published.

## Packages

- Packages come from Nix, not `package.el`: `early-init.org` sets `package-enable-at-startup nil`, and
  `home-manager/editor/packages/default.nix` calls `emacsWithPackagesFromUsePackage` with an empty config,
  so nothing in init.org selects packages. Add a new package to the matching list under
  `home-manager/editor/packages/epkgs/packages/<category>/default.nix`; per-package build fixes go in
  `override` in `home-manager/editor/packages/default.nix`.
- init.org does not use `use-package` or `leaf`. It uses its own `autoload-if-found` (defined near the
  top of init.org) plus `with-eval-after-load`. `autoload-if-found` returns nil and defines nothing when
  `locate-library` misses, so a package missing from the Nix lists produces no error, only unbound
  commands. Do not pass macros to it: byte-compiled callers then treat the macro as a function call.
- MELPA names drift between nixpkgs pins; check that the `epkgs` attribute exists at the pinned
  emacs-overlay with `nix eval` before relying on a package name from memory.

## Checking a change without the user's daemon

Work on a scratch tangle in a `mktemp -d` directory. Never byte-compile inside `~/.emacs.d`: `init.el`
there is a store symlink with an mtime of 1970, so a stray `init.elc` is always newer
and shadows every later switch.

Tangle exactly as the build does, from the repository root. `org` must be an absolute path; for the
working copy it is `$PWD/home-manager/editor/emacs/elisp/init.org`:

```bash
dir=$(mktemp -d); mkdir "$dir/home"
rev=$(jq -r '.nodes["org-babel"].locked.rev' flake.lock)
nix eval --raw --impure --expr "(builtins.getFlake \"github:emacs-twist/org-babel/$rev\").lib.tangleOrgBabel { languages = [ \"emacs-lisp\" ]; } (builtins.readFile $org)" > "$dir/init.el"
```

For a baseline, repeat with `base` in place of `dir` and `org=$base/init.org`, after writing trunk's
version with
`jj --ignore-working-copy file show -r 'trunk()' home-manager/editor/emacs/elisp/init.org > "$base/init.org"`.
Compile both the same way.

- Byte-compile: `(cd "$dir" && HOME="$dir/home" emacs -Q --batch -f batch-byte-compile init.el) > "$dir/compile.log" 2>&1`.
  `-Q` still sees the Nix-provided packages because the `emacs` wrapper sets the load path. The tree
  already emits hundreds of warnings, so judge a change by diffing warnings against the baseline with line
  numbers stripped:
  `diff <(sed 's/^init\.el:[0-9]*:[0-9]*: //' "$base/compile.log" | sort) <(sed 's/^init\.el:[0-9]*:[0-9]*: //' "$dir/compile.log" | sort)`.
- Do not trust a clean compile inside the user's Emacs: `early-init.org` sets
  `byte-compile-warnings '(cl-functions)`, a category Emacs 31 no longer has, which suppresses every
  warning. The `-Q` batch compile does not load early-init.
- Balance: `paredit inspect check --file "$dir/init.el"` exits 1 on unbalanced input. It cannot parse
  `.org` files, so run it on the tangle, not on init.org. Without paredit, run a read loop in `$dir`
  that skips comments first, so a missing paren signals instead of ending the loop:
  `emacs -Q --batch --eval '(with-temp-buffer (insert-file-contents "init.el") (set-syntax-table emacs-lisp-mode-syntax-table) (goto-char (point-min)) (let ((n 0)) (while (progn (forward-comment (buffer-size)) (not (eobp))) (read (current-buffer)) (setq n (1+ n))) (message "top-level forms: %d" n)))'`
  exits 255 on unbalanced input. A bare `(while t (read ...))` with an `end-of-file` handler reports a
  short count and exits 0 on a missing paren, and a `forward-sexp` loop never ends at end of buffer.
- `check-parens` on the tangle needs `(emacs-lisp-mode)` first. In `fundamental-mode` it reports
  "Unmatched bracket or quote" on this file even when the file is balanced.
- For editing Lisp in a tangle or test file, see the paredit-cli skill. init.org itself is Org, so edit
  its blocks as text and check the tangle. Serena has no Emacs Lisp or Org support; use grep and paredit-cli.

## Tests

- `home-manager/editor/emacs/tests/*.el` read named defuns out of init.org and `eval` them, so they test
  the current source without loading the whole config. `checks.emacs-ghq-jj` in `flake.nix` runs
  `ghq-jj-test.el`; `org-roam-db-autosync-missing-repo-guard-test.el` is run by hand (README.org).
  Wire a new test into `flake.nix` `checks` the same way rather than leaving it unrun.
- Run ERT from a working directory outside `$HOME`. `ghq-jj-test.el` rebinds `HOME`, and a
  `default-directory` such as `~/ghq/...` then resolves into the sandbox and most of its tests fail with
  `file-missing`. This passes:
  `cd /tmp && emacs --batch -l <repo>/home-manager/editor/emacs/tests/ghq-jj-test.el -f ert-run-tests-batch-and-exit`.

## The user's running daemon

- On macOS the daemon is the launchd agent `org.nix-community.home.emacs` (`--fg-daemon`, socket
  `/tmp/emacs$(id -u)/server`); on Linux it is a systemd user service with socket
  `/run/user/$(id -u)/emacs/server` (`home-manager/editor/emacs-service/default.nix`,
  `home-manager/editor/lib/emacs.nix`). It is the user's working editor.
- On macOS every home-manager activation restarts it (`launchctl kickstart -k` in the
  `restartEmacsDaemon` activation). `early-init.org` disables auto-save, so unsaved buffers are lost.
  Treat a switch as a daemon restart and ask first.
- Do not evaluate changed code, `elp-instrument-*`, or wholesale advice in the daemon through
  `emacsclient -e`: instrumentation has blocked it for over a minute. The repo's own liveness probe,
  `emacsclient -s <socket> -e '(emacs-pid)'`, is read-only; bound any client call (see CLAUDE.md
  standard_practices).
- To observe daemon-only behaviour (startup, `emacsclient -t` frames), start a separate named daemon:
  `TMPDIR=/tmp emacs --fg-daemon=<name> --init-directory=<scratch dir>` with the scratch tangle, connect with
  `emacsclient -s /tmp/emacs$(id -u)/<name>`, and stop only that one with `(kill-emacs)` through its socket.
- The daemon's environment, not the client's, decides tty frames: `TERMINFO_DIRS` for `xterm-kitty` is set
  on the launchd agent. A failure there shows only in the daemon's `*Messages*`.
- An error in `after-init-hook` runs before the daemon finishes initialising and kills it (exit 255, then
  a launchd restart loop); the same error in `emacs-startup-hook` leaves it running.
