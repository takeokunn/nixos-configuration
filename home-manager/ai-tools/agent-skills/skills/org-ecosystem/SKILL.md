---
name: org-ecosystem
description: Use for Org-mode, covering org files, syntax, babel, export, agenda, capture, GTD, literate programming, and publishing workflows.
version: 3.0.0
---

Org-mode workflows for GTD, literate programming with Babel, and export/publishing. Assumes
familiarity with headlines, basic markup, and standard block types — this covers the parts of Org
that surprise or bite: timestamp semantics, Babel's execution/security exposure, capture-template
mechanics, and export/agenda traps.

## Timestamps and repeaters

Active `<...>` timestamps appear in the agenda; inactive `[...]` timestamps do not. **This is the
only functional difference between the two syntaxes** — get it backwards and TODO items go missing
from the agenda, or bookkeeping timestamps start cluttering it.

Repeater cookies change what "next occurrence" means, not just the interval:

- `+1w` — jumps straight to now + 1 week, skipping any missed occurrences.
- `++1w` — cumulative: advances one interval at a time from the original date until the result is
  in the future, so a task missed for a month advances four separate weeks rather than jumping
  straight to "next week from today."
- `.+1w` — resets from the completion date rather than the original schedule; use for tasks like
  "review again in a week" where drift shouldn't compound.

## TODO states and capture

```org
#+TODO: TODO(t) NEXT(n) WAITING(w@/!) | DONE(d!) CANCELLED(c@)
```

`@` prompts for a note on state change, `!` logs a timestamp; `w@/!` logs a note on entry and a
timestamp on exit. States left of `|` are open, right of `|` are done — stuck-project detection
and clock reports key off this partition, not off individual state names.

Capture placeholders: `%?` cursor position, `%i` captured region, `%a` link back to the capture
origin, `%U`/`%T` inactive/active timestamp. `%a` is the one worth flagging — it silently embeds a
link to wherever point was when capture fired, useful for provenance but a dangling or absent link
when capture is triggered from a scratch buffer.

## Babel: execution and security

```elisp
(setq org-confirm-babel-evaluate nil)
```

**This disables the per-block confirmation prompt before evaluating code.** Once set, `C-c C-c`
inside any source block — or an export that triggers evaluation, depending on `:eval` — runs
arbitrary shell/python/elisp with no gate. Treat `.org` files from untrusted sources as
executable, not as documents; scope `org-confirm-babel-evaluate` per-directory with
`.dir-locals.el` instead of disabling it globally if you ever open files you didn't author.

A block only executes if its language was loaded via `org-babel-do-load-languages`; a
`#+BEGIN_SRC ruby` block with `ruby` absent from that list fails with "no org-babel-execute
function for ruby" even though the syntax highlights fine — check the loaded-languages list before
assuming a broken block is a syntax problem.

`:session` keeps a REPL alive across blocks, so state leaks between blocks sharing a session name —
a variable set in a later block is visible to an earlier one if that earlier block re-runs.
`:noweb yes` expands `<<name>>` references at *tangle or execute* time, not at edit time, so the
buffer's visible content and its executed content can diverge.

## Tangle and noweb

`:mkdirp yes` creates parent directories on tangle; omit it and tangling into a nonexistent
directory fails outright. Noweb references (`<<name>>`) resolve against blocks carrying a matching
`:noweb-ref`, not against `#+NAME:` — confusing the two is the usual reason a tangled file still
contains a literal `<<placeholder>>`.

## Export

`H:N` in `#+OPTIONS` sets which heading level becomes a Beamer frame boundary — it is easy to
assume `H:` maps to LaTeX's `\section` depth the way it does for other backends, but for Beamer it
controls frame splitting instead.

`org-latex-pdf-process` runs an external shell command sequence (`latexmk`, optionally
`-shell-escape`); the same untrusted-content caution as Babel applies when `-shell-escape` is
enabled, since LaTeX can execute code under that flag too.

`:noexport:` tags and `#+BEGIN_COMMENT` blocks are excluded from export output but still parsed —
a `:noexport:` heading with a live source block still executes on `C-c C-c` and still executes
during export-triggered evaluation; it just never appears in the rendered document.

org-cite (built into Org since 9.5, bundled with Emacs 30.2) replaces org-ref. The two conflict on
citation-key parsing when both are loaded, so migrate rather than running them side by side.

## Agenda performance

`org-agenda-files` is scanned in full on every agenda build — there is no incremental index — so a
large or deeply nested file set makes every `org-agenda-custom-commands` invocation noticeably
slower. Point it only at files that actually carry schedulable items, not at an entire notes tree.

## Refile

`(setq org-refile-allow-creating-parent-nodes 'confirm)` lets refile create a new heading on the
fly. Setting it to `t` instead of `'confirm` means a typo in the refile target silently creates a
stray heading rather than prompting first.

## org-roam

```elisp
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode))
```

`org-roam-db-autosync-mode` keeps the SQLite cache in sync only with edits made through Emacs.
Changes from outside Emacs — another editor, a script, `git checkout` — do not trigger a resync;
run `org-roam-node-sync`/`org-roam-db-sync` explicitly afterward, or node lookups silently return
stale results.

## Related

- [emacs-ecosystem](../emacs-ecosystem/SKILL.md) — Emacs Lisp mechanics (use-package, hooks) that
  org-mode customization is built on.
- [serena-usage](../serena-usage/SKILL.md) — symbol operations for navigating org structures
  programmatically.
- [context7-usage](../context7-usage/SKILL.md) — fetch current Org-mode documentation when a
  pattern here may be stale against the latest release.
- [technical-documentation](../technical-documentation/SKILL.md) — documentation patterns
  applicable to org export output.
