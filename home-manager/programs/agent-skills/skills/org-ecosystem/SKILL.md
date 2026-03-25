---
name: org-ecosystem
description: "Use when working with 'org-mode', 'org file', '.org', 'org syntax', 'org babel', 'org export', 'org agenda', 'org capture', 'GTD', or 'literate programming'. Provides Org-mode document patterns, GTD workflow, Babel literate programming, and export/publishing best practices."
---

Comprehensive patterns for Org-mode document creation, GTD workflow management, literate programming with Babel, and multi-format export/publishing.

## Org Syntax Fundamentals

### Headings and Properties

Org uses hierarchical outline structure with stars. The agent should attach properties as key-value metadata to headings:

```org
* Task
:PROPERTIES:
:CATEGORY: work
:EFFORT:   2h
:END:
```

### Timestamps

| Purpose | Syntax | Notes |
|---------|--------|-------|
| Active (shows in agenda) | `<2024-01-15 Mon>` | Schedule tasks |
| Inactive (no agenda) | `[2024-01-15 Mon]` | Record dates |
| With time | `<2024-01-15 Mon 10:00>` | Specific time |
| Repeating weekly | `<2024-01-15 Mon +1w>` | Fixed schedule |
| Restart from completion | `<2024-01-15 Mon .+1d>` | Reset repeater |
| Cumulative repeater | `<2024-01-15 Mon ++1d>` | Catch up missed |

### Tables with Formulas

```org
| Name  | Quantity | Price |  Total |
|-------+----------+-------+--------|
| Item1 |        2 |  10.0 |   20.0 |
| Item2 |        3 |  15.0 |   45.0 |
|-------+----------+-------+--------|
| Total |          |       |   65.0 |
#+TBLFM: $4=$2*$3::@>$4=vsum(@2..@-1)
```

Recalculate with `C-c C-c`. Create table from region with `C-c |`.

### Links

| Target | Syntax |
|--------|--------|
| External URL | `[[https://...][description]]` |
| Local file | `[[file:./path][description]]` |
| Heading in same file | `[[*Heading Name][description]]` |
| Stable cross-file ref | `[[id:uuid][description]]` |
| Named target | `<<target>>` and `[[target]]` |

### Text Markup

`*bold*`, `/italic/`, `_underline_`, `=verbatim=`, `~code~`, `+strikethrough+`

## GTD Workflow

### TODO State Configuration

```org
#+TODO: TODO(t) NEXT(n) WAITING(w@/!) | DONE(d!) CANCELLED(c@)
```

`@` prompts for note, `!` records timestamp, `|` separates active from done states.

**State selection:** TODO (not started) -> NEXT (next action) -> WAITING (blocked) -> DONE/CANCELLED.

### Capture Templates

```elisp
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
     "* TODO %?\n  %i\n  %a")
    ("n" "Note" entry (file "~/org/notes.org")
     "* %? :note:\n  %U\n  %i")
    ("j" "Journal" entry (file+datetree "~/org/journal.org")
     "* %?\n  %U")))
```

Template placeholders: `%?` cursor, `%i` active region, `%a` annotation, `%U` inactive timestamp.

### Agenda and Custom Views

```elisp
(setq org-agenda-files '("~/org/"))
(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-agenda-span 7)))
      (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
      (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))))))
```

### Tags and Clocking

```org
#+TAGS: @home(h) @work(w) @phone(p) @computer(c)
#+TAGS: urgent(u) important(i)

* TODO Call dentist :@phone:urgent:
:LOGBOOK:
CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 12:30] =>  2:30
:END:
```

Clock in: `C-c C-x C-i`, clock out: `C-c C-x C-o`, clock report: `C-c C-x C-r`.

## Babel (Literate Programming)

### Code Blocks and Header Arguments

```org
#+NAME: example-block
#+BEGIN_SRC python :results output :exports both
print("Hello from Python")
for i in range(3):
    print(f"  Item {i}")
#+END_SRC
```

Key header arguments: `:results` (value/output/silent), `:exports` (code/results/both/none), `:var` (variable binding), `:dir` (working directory), `:session` (persistent session), `:tangle` (extract to file), `:noweb` (reference expansion).

### Tangling (Code Extraction)

```org
#+BEGIN_SRC python :tangle ./script.py :shebang "#!/usr/bin/env python3"
def main():
    print("Generated from org file")

if __name__ == "__main__":
    main()
#+END_SRC
```

Tangle with `C-c C-v t`. Use `:mkdirp yes` to create directories.

### Noweb References

```org
#+NAME: imports
#+BEGIN_SRC python :noweb-ref imports
import os
import sys
#+END_SRC

#+BEGIN_SRC python :tangle ./program.py :noweb yes
<<imports>>

def main():
    print("Running main")

if __name__ == "__main__":
    main()
#+END_SRC
```

### Inline Code

```org
The result is src_python{return 2 + 2} {{{results(=4=)}}}.
Today is src_elisp{(format-time-string "%Y-%m-%d")}.
```

### Babel Language Setup

```elisp
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t) (python . t) (shell . t) (js . t) (sql . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
```

## Export and Publishing

### Export Backends

| Target | Backend | Command |
|--------|---------|---------|
| Web page | ox-html | `C-c C-e h h` |
| PDF | ox-latex | `C-c C-e l p` |
| Slides | ox-beamer | `C-c C-e l P` |
| Markdown | ox-md / ox-gfm | `C-c C-e m m` |
| Word doc | ox-odt | `C-c C-e o o` |

### Document Header

```org
#+TITLE: Document Title
#+AUTHOR: Author Name
#+DATE: 2024-01-15
#+OPTIONS: toc:2 num:t author:t
```

### Publishing Projects

```elisp
(setq org-publish-project-alist
  '(("org-notes"
     :base-directory "~/org/notes/"
     :base-extension "org"
     :publishing-directory "~/public_html/"
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4
     :auto-preamble t)
    ("org-static"
     :base-directory "~/org/notes/"
     :base-extension "css\\|js\\|png\\|jpg\\|gif"
     :publishing-directory "~/public_html/"
     :recursive t
     :publishing-function org-publish-attachment)
    ("org" :components ("org-notes" "org-static"))))
```

Use `:noexport:` tag to exclude headings. Use `#+BEGIN_COMMENT` blocks for non-exported content.

## Workflow

1. **Analyze**: Identify document type (notes, project, literate program, publication) and required features
2. **Implement**: Set up document header, structure headings, add metadata and babel blocks
3. **Validate**: Check with `org-lint`, test babel execution, verify export output

## Best Practices

- Use one file per major project or area of responsibility
- Keep `inbox.org` for quick captures, refile regularly (daily or weekly)
- Use consistent TODO state workflow across all files
- Use tags for context (`@home`, `@work`) not categories
- Use `org-id` for stable cross-file links
- Archive completed subtrees periodically

## Anti-Patterns

- **Single giant file**: Split by project/area; use `org-agenda-files`
- **Deep nesting (>4-5 levels)**: Refactor into separate files or flatten
- **Hardcoded paths**: Use relative paths or `org-directory` variable
- **Babel side effects**: Use `:results silent` for side-effect blocks, document clearly

## Critical Rules

- The agent should never skip heading levels
- The agent should close all drawers and blocks properly
- The agent should use consistent timestamp format throughout
- The agent should add `:PROPERTIES:` drawer after heading, not before
- The agent should use `#+NAME:` before code blocks that are referenced
