---
name: melpa-packaging
description: Use when preparing an Emacs Lisp package for MELPA submission - recipe review, :files selection, package-lint/checkdoc findings, package header alignment (Package-Requires, Version, URL), and release-gate hygiene (non-mutating lint/compile targets, isolated byte-compilation).
version: 3.0.0
---

Shipping a MELPA-quality Emacs Lisp package means keeping the recipe, the main library's
headers, and the local lint/compile targets in agreement with what package-build actually
generates. The guidance below is organized around three mechanisms that drive most of the
rules: what the build generates for you, how it computes the version, and how the recipe's
`:files` list defines exactly what a user receives.

## Mental model

**package-build derives distributable artifacts from source; it does not ship your working
tree verbatim.** It generates the package descriptor (`NAME-pkg.el`) from the main library's
headers, generates autoloads, and stamps a version from the repository. Anything the build
generates must never be committed — a committed copy either gets overwritten or ships stale
and conflicts with the generated one. Treat the main library's comment headers as the single
source of truth for package identity; the descriptor, autoloads, and version all flow from
there mechanically.

**The version is computed, not declared.** For the default (unstable) channel, package-build
stamps the version from the date of the latest commit touching a file the recipe selects,
formatted `%Y%m%d`. For the stable channel it reads a matching SCM tag (parsed by
`version-to-list`, adjustable with `:version-regexp`). The `Version:` header you write is a
floor/label, not the published number — keep it present and monotonic anyway, since
package.el, package-lint, and tools that read source directly all rely on it.

**The recipe's `:files` list is the exact boundary of what users receive.** Anything excluded
does not exist for an installed user; anything included ships whether or not it belongs to
the feature. Specify only what the feature needs to run, then make your local byte-compile,
checkdoc, and package-lint targets operate on that same set, so what you verify locally is
what users actually get.

## Recipe format

A recipe is a single file in the archive's `recipes/` directory, named exactly after the
package (no extension) and matching the main library's feature name. Its contents are one
Lisp form: `(NAME :fetcher ... KEYWORDS...)`.

```elisp
;; Simplest correct recipe: default :files, latest commit drives the unstable version.
;; :fetcher may be github, gitlab, codeberg, sourcehut, or git/hg with an explicit :url.
(foo :fetcher github :repo "owner/foo")

;; Sources under a non-default directory (e.g. src/): declare :files explicitly and
;; keep the trailing (:exclude ...) element — a bare list without it REPLACES, not
;; extends, the default exclusions.
(foo :fetcher github :repo "owner/foo"
  :files ("src/*.el" (:exclude "src/foo-autoloads.el")))

;; :branch overrides the default branch for the unstable build.
;; :version-regexp lets the stable channel parse tags like v1.2.0 by capturing the
;; numeric part; the captured group is passed to version-to-list.
(foo :fetcher github :repo "owner/foo"
  :branch "main" :version-regexp "v?\\(.*\\)")
```

Submission is a pull request that adds only the one recipe file. Reviewers commonly build it
locally (the archive provides a `make recipes/NAME` target and a sandboxed-install target)
before merging, so **the recipe must build cleanly against the current repository HEAD, not a
local uncommitted tree.**

## Package headers

Never commit a `NAME-pkg.el`: its description, version, and requires are all read from the
main library's headers, so a hand-written descriptor is redundant at best and divergent at
worst.

Put package metadata in the comment headers of the main library (the file named after the
package): `Package-Requires`, `Version`, `URL`, `Keywords`, `Author`, `Maintainer`. Headers
must follow the `package.el` header format from the Emacs Lisp manual's Packaging section.
`Package-Requires` is read literally to compute dependencies, so it must list every
dependency and a realistic minimum version:

```elisp
;;; foo.el --- Do a useful thing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Author Name

;; Author: Author Name <author@example.com>
;; Maintainer: Author Name <author@example.com>
;; URL: https://github.com/owner/foo
;; Version: 1.2.0
;; Keywords: convenience tools
;; Package-Requires: ((emacs "29.1") (dash "2.19"))

;; This file is NOT part of GNU Emacs.

;; ... (full GPLv3 notice) ...

;;; Commentary:
;;
;; One or more paragraphs describing what the package does.

;;; Code:

;; ... definitions ...

(provide 'foo)
;;; foo.el ends here
```

- Summary line (`;;; foo.el --- description`) stays under 80 characters, with the
  `lexical-binding` cookie in the `-*- -*-` block — this cookie is required on every shipped
  `.el` file, on the first line.
- `Keywords` are space-separated, drawn from the standard finder keyword list
  (`M-x finder-list-keywords`), and placed before `Package-Requires`.
- End the file with `(provide 'foo)` followed by `;;; foo.el ends here`; both are
  checkdoc/package-lint expectations.

## `:files` selection

**Understand the default set before overriding it.** package-build by default selects
top-level and `lisp/` Elisp plus info/texinfo docs, and already excludes dotfiles and test
files (`test.el`, `tests.el`, `*-test.el`, `*-tests.el`, and their `lisp/` equivalents):

```elisp
("*.el" "lisp/*.el"
 "dir" "*.info" "*.texi" "*.texinfo"
 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
 (:exclude ".*.el" "lisp/.*.el"
           "test.el" "tests.el" "*-test.el" "*-tests.el"
           "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
```

If sources sit under `lisp/` and tests match `*-test(s).el`, no `:files` override is needed
at all. When source libraries live outside the default-selected locations (e.g. `src/`),
declare `:files` explicitly and exclude generated files such as a committed
`*-autoloads.el`. Prefer a narrow inclusion with an explicit exclusion over a broad wildcard,
e.g. `:files ("src/*.el" (:exclude "src/NAME-autoloads.el"))`; enumerate the production
modules if a wildcard would sweep in generated or non-shipping files.

Decision guide:

| Situation | Action |
|---|---|
| Sources top-level or under `lisp/`, tests match `*-test(s).el` | No override — default already covers it |
| Sources under a non-default dir such as `src/` | Explicit `:files` reaching those sources, excluding generated files |
| Info/texinfo docs from a custom location | Extend `:files` with the doc globs, kept minimal |
| A generated file (autoloads) is tracked in the repo | Exclude it explicitly so it never ships |

**Keep local targets aligned with the recipe.** Byte-compile and lint exactly the file set the
recipe ships, and point package-lint at the main file explicitly (`package-lint-main-file`,
or `package-lint-batch-and-exit` with the main file first) so secondary modules are checked in
the correct context — linting only the main library hides naming, custom-group, and docstring
issues elsewhere. This is a strong practice observed across well-maintained repos, not an
automatically enforced MELPA rule.

Test files are not part of the package and are excluded from the shipped set by default; they
must not `provide` package features. When tests need shared helpers, load them by path (e.g.
via `load` under `eval-and-compile`, made idempotent) instead of adding `provide`/`require`
coupling. If an existing harness already relies on feature-based `require` between test files,
convert the loader to idempotent path-based loading before removing those `provide` forms, or
the suite will fail to load.

Until the recipe is merged and the package is live on MELPA, documentation must not imply
`package-install` works from MELPA. Phrase availability conditionally ("Once available on
MELPA…") and document a working alternative such as `package-vc-install` or a `use-package`
`:vc` recipe; apply the same care to changelog and section headings.

## Release-gate hygiene

**A release gate (`make check`, `make lint`, the CI target contributors run) must not mutate
the working tree or the contributor's Emacs package directory.** It reports; it does not
install, refresh, or leave artifacts behind. Three obligations follow:

- Byte-compilation and autoload validation happen in a temporary directory, so the gate does
  not deposit `.elc` files or a generated `NAME-autoloads.el` in the tree — stale bytecode
  then shadows edited source in later sessions, and a stray generated descriptor recreates the
  committed-artifact problem the recipe conventions exist to prevent.
- Validate autoloads by generating them (`loaddefs-generate`) and asserting the expected
  public autoload forms are present. **Do not infer the result from `git diff`** — generated
  files are typically ignored or untracked, so the diff reads clean whether or not the
  autoloads are correct.
- No gate refreshes a package archive or auto-installs dependencies into the contributor's
  package directory: a check that reaches the network and mutates a personal Emacs
  installation is not a check. When a required tool is missing, fail with a message naming it.

Gates conventionally invoke `emacs -Q` so no user configuration influences the result — which
also means no user-installed packages are visible. package-lint and any other tooling the gate
runs must be supplied by the declared development environment (a project shell, container, or
an Emacs built with those packages); a plain Emacs binary makes a documented
`make package-lint` target fail even though the package itself is fine, so the tooling
dependency belongs in the environment definition next to the target that needs it.

## Static checks

Run both tools on every shipped file before submitting. These are the concrete failures you
will see, distinct from human-review style requests.

**package-lint** checks:
- `lexical-binding: t` cookie present on the first line.
- Header summary line and `;;; NAME.el ends here` footer exist and are well-formed.
- `Package-Requires` present and parseable when dependencies are used.
- Each declared dependency is available from a configured package archive with the pinned
  version actually existing (no non-existent or snapshot-only versions).
- The declared `emacs` minimum matches the newest built-in symbol the code uses —
  package-lint knows which functions/variables were introduced in which Emacs version and
  demands you raise the floor or drop the call. This accuracy check is its central value.
- Every defined symbol is namespaced with the package prefix; unprefixed
  `defun`/`defvar`/`defcustom`/`defface`/custom groups are flagged.
- No reserved keybindings are bound (e.g. the user-reserved `C-c <letter>` space).
- `cl` is not `require`d directly; use `cl-lib`.

Designate the main file when linting a multi-file package so cross-file prefix and dependency
checks resolve correctly.

**checkdoc** checks:
- Each docstring's first line is a single complete sentence in the imperative mood
  ("Toggle…", not "Toggles…" or "This toggles…").
- Symbols referenced in docstrings are quoted as `` `symbol' ``; sentences end with two
  spaces before the next.
- Interactive commands and public defuns/defvars have docstrings; the summary line stays
  within width.
- The file has the expected section comments (`;;; Commentary:`, `;;; Code:`) and footer.

checkdoc governs docstring form; it does not enforce predicate-naming or "not Global"
phrasing — those come from human reviewers applying Elisp conventions.

## Review findings

Recurring items raised in MELPA human review and by package-lint/checkdoc, split into
blocking (the recipe will not be accepted, or the package misbehaves) and deferrable (style or
design calls that can wait). **Do not silence a finding by weakening a real design decision —
record why it is deferred instead.**

Blocking:
- **Headers** — missing `Maintainer:`, or non-standard ad-hoc headers. Add `Maintainer:`
  alongside `Author:`; remove invented headers; keep `Keywords` space-separated before
  `Package-Requires`.
- **URLs** — `http://` or an unreachable homepage. Use a canonical `https://` project URL.
- **License boilerplate** — inconsistent across files, or a file claims "This file is part of
  GNU Emacs." Use "This file is NOT part of GNU Emacs." and keep the same GPL block and
  version line across the main library and every shipped file; match the main file's license
  version rather than mixing GPL-2+ and GPL-3+.
- **Buffer-local vars** — `make-variable-buffer-local` used to declare one. Prefer
  `defvar-local`, the modern idiom reviewers expect.
- **Docstrings** — non-imperative command summaries, mislabeled scope, wrong predicate
  phrasing. Use imperative mood for commands; do not describe a buffer-local minor mode as
  "Global"; phrase predicate functions (trailing `-p`/`p`) as a question about what returns
  non-nil. Resolve every checkdoc warning on shipped files.
- **Prefix naming** — package-lint flags symbols without the package prefix, or reserved
  keybindings. This is a genuine installability/coding-convention check — treat it as
  blocking, not cosmetic.

Deferrable:
- **Design suggestions** that touch behavior — redundant helper commands, cleanup-on-disable
  semantics, calling the real `widen`, missing large-buffer performance tests. If a suggestion
  conflicts with an intended design, keep the design and note the rationale in the pull
  request rather than changing behavior to satisfy a non-blocking comment; track genuine
  nice-to-haves separately so they are not lost.

## Pre-submission checklist

- Main library headers present and correct: summary line with `lexical-binding: t`,
  `Author`, `Maintainer`, `URL` (https), `Version`, space-separated `Keywords`, then
  `Package-Requires` with every dependency and a realistic minimum.
- No `NAME-pkg.el` committed; no committed generated autoloads shipped.
- `Package-Requires` lists an `emacs` minimum matching the APIs actually used.
- Every shipped file carries the `lexical-binding` cookie, a matching `provide`,
  `Commentary`/`Code` sections, and consistent GPL boilerplate.
- package-lint passes on every shipped file with the main file designated; checkdoc passes
  on every shipped file.
- Package byte-compiles cleanly.
- Recipe `:files` includes only what the feature needs; default set used unmodified where
  possible; tests and generated files excluded.
- Local compile/lint targets operate on the same file set the recipe ships.
- Release-gate targets leave the working tree unchanged: byte-compilation and autoload
  generation happen in a temporary directory, no `.elc` or generated autoload file lands in
  the repository.
- Autoload validation asserts expected public autoload forms from a generated file rather
  than inferring success from `git diff`.
- No gate refreshes a package archive or installs packages into the contributor's package
  directory; the development environment supplies package-lint and any other `emacs -Q`
  tooling.
- Test files provide no package features; shared test helpers load by path, idempotently.
- README/changelog make no MELPA-availability claim; a `package-vc`/`use-package :vc`
  install path is documented for the pre-acceptance window.
- For stable-channel intent, an SCM tag exists in a form `version-to-list` accepts (add
  `:version-regexp` if the tag is prefixed).

## Related

- [emacs-ecosystem](../emacs-ecosystem/SKILL.md) — Emacs Lisp language, package.el, and editor-integration foundations
- [lisp-macro](../lisp-macro/SKILL.md) — macro-heavy source that must still satisfy package-lint prefix and docstring checks
- [technical-writing](../technical-writing/SKILL.md) — README, Commentary, and changelog wording during the pre-acceptance window
- [context7-usage](../context7-usage/SKILL.md) — fetch current package-lint / package.el documentation when a check is ambiguous
