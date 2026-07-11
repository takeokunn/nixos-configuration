---
name: melpa-packaging
description: This skill should be used when the user prepares an Emacs Lisp package for MELPA submission, writes or reviews a MELPA recipe, decides which files ship in a package, resolves package-lint or checkdoc findings, aligns package headers (Package-Requires, Version, URL, Author, Maintainer), or handles the period between opening a MELPA pull request and acceptance. Trigger on mentions of MELPA, recipe, :files, package-build, package-lint, checkdoc, or *-pkg.el.
version: 2.0.0
---

<purpose>
  Provide durable principles for shipping a MELPA-quality Emacs Lisp package: what the
  build system does on your behalf, what belongs in source versus what is generated,
  how to keep the recipe and the local lint/compile targets in agreement, and how to
  triage the recurring review findings. The emphasis is on the reasoning ("why the rule
  exists") so the guidance transfers to any package layout, followed by concrete examples.
</purpose>

<mental_model>
  <principle name="build_system_owns_generated_metadata">
    <why>
      MELPA's package-build derives the distributable artifacts from your source; it does
      not ship your working tree verbatim. It generates the package descriptor
      (`NAME-pkg.el`) from headers in the main library, generates autoloads, and stamps a
      version from the repository. Anything the build generates is something you should not
      author or commit, because a committed copy either gets overwritten or, worse, ships
      stale and conflicts with the generated one.
    </why>
    <implication>
      Treat the main library's comment headers as the single source of truth for package
      metadata. Everything else about identity (descriptor, autoloads, version) flows
      from there mechanically.
    </implication>
  </principle>

  <principle name="version_is_computed_not_declared">
    <why>
      For the default (unstable) channel, package-build stamps the version from the date of
      the latest commit that touched a file the recipe selects, formatted `%Y%m%d`. For the
      stable channel it reads a matching SCM tag (parsed by `version-to-list`, adjustable
      with `:version-regexp`). The `Version:` header you write is a floor/label, not the
      published number.
    </why>
    <implication>
      Do not expect your `Version:` header to appear verbatim on MELPA unstable. Keep it
      present and monotonic anyway — package.el and package-lint still read it, and tools
      that consume the source directly rely on it.
    </implication>
  </principle>

  <principle name="the_recipe_is_a_file_contract">
    <why>
      The recipe's `:files` list is the exact boundary of what users receive. Anything you
      exclude does not exist for an installed user, and anything you include ships whether or
      not it is really part of the feature. A package that pulls in tests, fixtures, or build
      scaffolding is larger and riskier than it needs to be.
    </why>
    <implication>
      Specify only what the feature needs to run. Then make your local byte-compile,
      checkdoc, and package-lint targets operate on exactly that same set, so what you verify
      locally is what users actually get.
    </implication>
  </principle>
</mental_model>

<recipe_format>
  <description>
    A recipe is a single file in the archive's `recipes/` directory, named exactly after the
    package (no extension). Its contents are one Lisp form: `(NAME :fetcher ... KEYWORDS...)`.
    The filename must equal the package name and the main library's feature name.
  </description>

  <example name="github_simple">
    <code>(foo :fetcher github :repo "owner/foo")</code>
    <notes>
      <item>The simplest correct recipe: default `:files`, latest commit drives the unstable version.</item>
      <item>`:fetcher` may be `github`, `gitlab`, `codeberg`, `sourcehut`, or `git`/`hg` with an explicit `:url`.</item>
    </notes>
  </example>

  <example name="custom_files">
    <code>(foo :fetcher github :repo "owner/foo"
      :files ("src/*.el" (:exclude "src/foo-autoloads.el")))</code>
    <notes>
      <item>Needed when sources live under a non-default directory such as `src/`.</item>
      <item>Keep the trailing `(:exclude ...)` element to drop generated files; a bare list without it replaces (does not extend) the default exclusions.</item>
    </notes>
  </example>

  <example name="branch_and_version_regexp">
    <code>(foo :fetcher github :repo "owner/foo"
      :branch "main" :version-regexp "v?\\(.*\\)")</code>
    <notes>
      <item>`:branch` overrides the default branch for the unstable build.</item>
      <item>`:version-regexp` lets the stable channel parse tags like `v1.2.0` by capturing the numeric part; the captured group is passed to `version-to-list`.</item>
    </notes>
  </example>

  <detail>
    Submission is a pull request that adds only the one recipe file. Reviewers commonly build
    it locally (the archive provides a `make recipes/NAME` target and a sandboxed-install
    target) before merging, so the recipe must build cleanly against the current repository
    HEAD, not a local uncommitted tree.
  </detail>
</recipe_format>

<conventions>
  <convention name="no_pkg_el_in_vcs" confidence="verified">
    <rule>Never commit a `NAME-pkg.el`. It is generated from the main library's metadata.</rule>
    <detail>
      The descriptor's description, version, and requires are all read from the main
      library's headers, so a hand-written descriptor is redundant at best and divergent at
      worst.
    </detail>
  </convention>

  <convention name="metadata_in_main_library" confidence="verified">
    <rule>
      Put package metadata in the comment headers of the main library (the file named after
      the package): `Package-Requires`, `Version`, `URL`, `Keywords`, `Author`, `Maintainer`.
    </rule>
    <detail>
      Headers must follow the `package.el` header format described in the Emacs Lisp manual's
      Packaging section. `Package-Requires` is read literally to compute dependencies, so it
      must list every dependency and a realistic minimum version, e.g.
      `((emacs "29.1") (some-dep "1.2"))`.
    </detail>
    <example name="main_library_header">
      <code>
        ;;; foo.el --- Do a useful thing  -*- lexical-binding: t; -*-

        ;; Copyright (C) 2026  Author Name

        ;; Author: Author Name &lt;author@example.com&gt;
        ;; Maintainer: Author Name &lt;author@example.com&gt;
        ;; URL: https://github.com/owner/foo
        ;; Version: 1.2.0
        ;; Keywords: convenience tools
        ;; Package-Requires: ((emacs "29.1") (dash "2.19"))

        ;; This file is NOT part of GNU Emacs.

        ;; This program is free software: you can redistribute it and/or modify
        ;; ... (full GPLv3 notice) ...

        ;;; Commentary:
        ;;
        ;; One or more paragraphs describing what the package does.

        ;;; Code:

        ;; ... definitions ...

        (provide 'foo)
        ;;; foo.el ends here
      </code>
      <notes>
        <item>Summary line `;;; foo.el --- description` must stay under 80 characters, with the `lexical-binding` cookie in the `-*- -*-` block.</item>
        <item>`Keywords` are space-separated and should be drawn from the standard finder keyword list (`M-x finder-list-keywords`); place them before `Package-Requires`.</item>
        <item>End the file with `(provide 'foo)` followed by the `;;; foo.el ends here` footer line; both are checkdoc/package-lint expectations.</item>
      </notes>
    </example>
  </convention>

  <convention name="lexical_binding_required" confidence="verified">
    <rule>Every shipped `.el` file must begin with a `lexical-binding: t` file-local cookie.</rule>
    <detail>The cookie belongs on the first line, in the `-*- ... -*-` block after the summary.</detail>
  </convention>

  <convention name="default_file_set_and_test_exclusion" confidence="verified">
    <rule>
      Understand the default `:files` set before overriding it. By default package-build
      selects top-level and `lisp/` Elisp plus info/texinfo docs, and it already excludes
      dotfiles and test files (`test.el`, `tests.el`, `*-test.el`, `*-tests.el`, and their
      `lisp/` equivalents).
    </rule>
    <detail>
      The default spec is roughly:
      <code>
        ("*.el" "lisp/*.el"
         "dir" "*.info" "*.texi" "*.texinfo"
         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
         "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
         (:exclude ".*.el" "lisp/.*.el"
                   "test.el" "tests.el" "*-test.el" "*-tests.el"
                   "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
      </code>
      If your sources sit under `lisp/` and your tests match `*-test(s).el`, you often need
      no `:files` at all. Add one only to change this set, and prefer the smallest override.
    </detail>
  </convention>

  <convention name="non_default_layout_needs_explicit_files" confidence="verified">
    <rule>
      If source libraries live outside the default-selected locations (for example under
      `src/`), the default file set will not pick them up. Declare `:files` explicitly, and
      exclude generated files such as a committed `*-autoloads.el`.
    </rule>
    <detail>
      Prefer a narrow inclusion with an explicit exclusion of generated files over a broad
      wildcard, e.g. `:files ("src/*.el" (:exclude "src/NAME-autoloads.el"))`. Enumerate the
      production modules if a wildcard would sweep in generated or non-shipping files.
    </detail>
  </convention>

  <convention name="recipe_and_local_targets_agree" confidence="scoped">
    <rule>
      Keep the set of files your Makefile (or equivalent) byte-compiles and lints identical
      to the set the recipe ships. Point package-lint at the main file explicitly so
      secondary modules are checked in the correct context.
    </rule>
    <detail>
      Linting only the main library hides naming, custom-group, and docstring issues in
      secondary modules. Run package-lint over every shipped file with the main file
      designated (package-lint offers a main-file setting for exactly this). This convention
      is a strong practice observed across well-maintained repos rather than an
      automatically enforced MELPA rule.
    </detail>
  </convention>

  <convention name="test_files_are_not_package_features" confidence="scoped">
    <rule>
      Test files are not part of the package. They should not provide package features and
      are excluded from the shipped set by default.
    </rule>
    <detail>
      When tests need shared helpers, load helper files by path (for example via `load` under
      `eval-and-compile`, making the load idempotent) instead of adding `provide`/`require`
      feature coupling to test files. If an existing harness already relies on
      feature-based `require` between test files, convert the loader to idempotent
      path-based loading before removing those `provide` forms, or the test suite will fail
      to load.
    </detail>
  </convention>

  <convention name="readme_claims_track_actual_acceptance" confidence="scoped">
    <rule>
      Until the recipe is actually merged and the package is live on MELPA, documentation
      must not imply `package-install` works from MELPA.
    </rule>
    <detail>
      During the pre-acceptance window, phrase availability conditionally ("Once available on
      MELPA…") and document a working alternative such as `package-vc-install` or a
      `use-package` `:vc` recipe. Apply the same care to changelog and section headings so
      they do not assert submission or acceptance before it has happened.
    </detail>
  </convention>
</conventions>

<static_checks>
  <description>
    What the two required tools actually verify. Run both on every shipped file before
    submitting. These are the concrete failures you will see, distinct from human-review
    style requests.
  </description>

  <tool name="package-lint" confidence="verified">
    <check>The `lexical-binding: t` cookie is present on the first line.</check>
    <check>The header summary line (`;;; NAME.el --- ...`) and the `;;; NAME.el ends here` footer exist and are well-formed.</check>
    <check>`Package-Requires` is present and parseable when dependencies are used.</check>
    <check>Each declared dependency is available from a configured package archive, and the pinned version actually exists (no non-existent or snapshot-only versions).</check>
    <check>The declared `emacs` minimum matches the newest built-in symbol the code uses — package-lint knows which functions/variables were introduced in which Emacs version and will demand you raise the floor (or drop the call). This accuracy check is package-lint's central value.</check>
    <check>Every defined symbol is namespaced with the package prefix; unprefixed `defun`/`defvar`/`defcustom`/`defface`/custom groups are flagged.</check>
    <check>No reserved keybindings are bound (for example the user-reserved `C-c <letter>` space).</check>
    <check>`cl` is not `require`d directly; use `cl-lib`.</check>
    <note>Designate the main file when linting a multi-file package (via `package-lint-main-file`, or `package-lint-batch-and-exit` with the main file first) so cross-file prefix and dependency checks resolve correctly.</note>
  </tool>

  <tool name="checkdoc" confidence="verified">
    <check>Each docstring's first line is a single complete sentence in the imperative mood ("Toggle…", not "Toggles…" or "This toggles…").</check>
    <check>Symbols referenced in docstrings are quoted as `` `symbol' ``; sentences end with two spaces before the next.</check>
    <check>Interactive commands and public defuns/defvars have docstrings; the summary line stays within width.</check>
    <check>The file has the expected section comments (`;;; Commentary:`, `;;; Code:`) and footer.</check>
    <note>checkdoc governs docstring form; it does not enforce the predicate-naming or "not Global" phrasing below — those come from human reviewers applying Elisp conventions.</note>
  </tool>
</static_checks>

<review_findings>
  <description>
    Recurring items raised in MELPA human review and by package-lint/checkdoc. Split them
    into blocking (the recipe will not be accepted, or the package misbehaves) and
    deferrable (style or design calls that can wait). Do not silence a finding by weakening a
    real design decision; record why it is deferred instead.
  </description>

  <finding category="headers" severity="blocking" confidence="scoped">
    <symptom>Missing `Maintainer:` header, or non-standard ad-hoc headers.</symptom>
    <fix>
      Add a `Maintainer:` header alongside `Author:`. Remove invented headers that are not
      part of the package.el header convention; keep `Keywords` space-separated and place it
      before `Package-Requires`.
    </fix>
  </finding>

  <finding category="urls" severity="blocking" confidence="scoped">
    <symptom>`URL:`/homepage uses `http://` or points somewhere unreachable.</symptom>
    <fix>Use a canonical `https://` project URL.</fix>
  </finding>

  <finding category="license_boilerplate" severity="blocking" confidence="scoped">
    <symptom>
      License boilerplate is inconsistent across files, or a file claims "This file is part
      of GNU Emacs."
    </symptom>
    <fix>
      Use "This file is NOT part of GNU Emacs." and keep the same GPL license block (and the
      same GPL version line) across the main library and every shipped file. When tests
      carry a header, match the main file's license version rather than mixing GPL-2+ and
      GPL-3+.
    </fix>
  </finding>

  <finding category="buffer_local_vars" severity="blocking" confidence="scoped">
    <symptom>`make-variable-buffer-local` used to declare a buffer-local variable.</symptom>
    <fix>
      Prefer `defvar-local` for variables intended to be buffer-local from the start; it is
      the modern idiom reviewers expect.
    </fix>
  </finding>

  <finding category="docstrings" severity="blocking" confidence="scoped">
    <symptom>Docstrings that checkdoc flags: non-imperative command summaries, mislabeled scope, wrong predicate phrasing.</symptom>
    <fix>
      Write command docstrings in the imperative mood ("Toggle…", not "Toggles…"). Do not
      describe a buffer-local minor mode as "Global". For predicate functions (named with a
      trailing `-p`/`p`), phrase the docstring as a question about what returns non-nil. Run
      checkdoc and resolve every warning it emits on shipped files.
    </fix>
  </finding>

  <finding category="prefix_naming" severity="blocking" confidence="verified">
    <symptom>package-lint flags symbols that do not use the package's prefix, or reserved keybindings.</symptom>
    <fix>
      Give every defined symbol the package prefix. This is a genuine installability/coding
      convention check, so treat prefix and keybinding warnings as blocking, not cosmetic.
    </fix>
  </finding>

  <finding category="deferrable_design" severity="deferrable" confidence="scoped">
    <symptom>
      Reviewer suggestions that touch behavior: redundant helper commands, cleanup-on-disable
      semantics, calling the real `widen`, missing large-buffer performance tests.
    </symptom>
    <fix>
      Distinguish these from blocking findings. If a suggestion conflicts with an intended
      design, keep the design and note the rationale in the pull request rather than changing
      behavior to satisfy a non-blocking comment. Track genuine nice-to-haves separately so
      they are not lost.
    </fix>
  </finding>
</review_findings>

<checklist>
  <description>Pre-submission gate. Everything here should pass before opening the recipe pull request.</description>
  <item confidence="verified">Main library headers present and correct: summary line with `lexical-binding: t`, `Author`, `Maintainer`, `URL` (https), `Version`, space-separated `Keywords`, then `Package-Requires` with every dependency and a realistic minimum.</item>
  <item confidence="verified">No `NAME-pkg.el` committed; no committed generated autoloads shipped.</item>
  <item confidence="verified">`Package-Requires` lists an `emacs` minimum that matches the APIs actually used.</item>
  <item confidence="verified">Every shipped file carries the `lexical-binding` cookie, a matching `provide`, `Commentary`/`Code` sections, and consistent GPL boilerplate.</item>
  <item confidence="verified">package-lint passes on every shipped file with the main file designated; checkdoc passes on every shipped file.</item>
  <item confidence="verified">Package byte-compiles cleanly.</item>
  <item confidence="verified">Recipe `:files` includes only what the feature needs; the default set is used unmodified where possible; tests and generated files are excluded.</item>
  <item confidence="scoped">Local compile/lint targets operate on the same file set the recipe ships.</item>
  <item confidence="scoped">Test files provide no package features; shared test helpers load by path, idempotently.</item>
  <item confidence="scoped">README/changelog make no MELPA-availability claim; a `package-vc`/`use-package :vc` install path is documented for the pre-acceptance window.</item>
  <item confidence="scoped">For stable-channel intent, an SCM tag exists in a form `version-to-list` accepts (add `:version-regexp` if the tag is prefixed).</item>
</checklist>

<anti_patterns>
  <avoid name="committing_generated_artifacts">
    <description>Checking in `NAME-pkg.el` or generated autoloads because "the build needs them".</description>
    <instead>Let package-build generate them. Keep only source; exclude any local generated copies via `:files`.</instead>
  </avoid>

  <avoid name="broad_files_wildcard">
    <description>Using an over-broad `:files` that ships tests, fixtures, or generated files.</description>
    <instead>Start from the default set. Override only to reach non-default source locations, and pair inclusions with explicit `:exclude` of generated files.</instead>
  </avoid>

  <avoid name="linting_only_the_main_file">
    <description>Running package-lint/checkdoc on the main library alone and assuming the rest is clean.</description>
    <instead>Lint every shipped file with the main file designated, so secondary-module naming and docstring issues surface.</instead>
  </avoid>

  <avoid name="premature_melpa_claims">
    <description>Documenting `package-install` from MELPA before the recipe is merged.</description>
    <instead>Phrase availability conditionally and document a VCS-based install until the package is live.</instead>
  </avoid>

  <avoid name="silencing_findings_by_weakening_design">
    <description>Changing real behavior only to make a non-blocking review comment go away.</description>
    <instead>Separate blocking from deferrable findings; keep intentional design and record the rationale.</instead>
  </avoid>

  <avoid name="test_files_providing_features">
    <description>Adding `provide`/feature coupling to test files so other tests can `require` them.</description>
    <instead>Load shared helpers by path idempotently; keep test files out of the package feature graph.</instead>
  </avoid>
</anti_patterns>

<decision_tree name="files_property">
  <question>Do you need a `:files` override at all?</question>
  <if condition="Sources are top-level or under lisp/, tests match *-test(s).el">No override — the default set already includes sources and excludes tests.</if>
  <if condition="Sources live under a non-default dir such as src/">Add an explicit `:files` that reaches those sources and excludes generated files.</if>
  <if condition="You ship info/texinfo docs from a custom location">Extend `:files` to include the doc globs; keep them minimal.</if>
  <if condition="A generated file (autoloads) is tracked in the repo">Exclude it explicitly so it never ships.</if>
</decision_tree>

<rules priority="critical">
  <rule>Metadata lives in the main library headers; never commit a generated descriptor or autoloads.</rule>
  <rule>Ship the minimal file set; keep local lint/compile targets aligned with the recipe.</rule>
  <rule>package-lint and checkdoc must pass on every shipped file, with the main file designated.</rule>
  <rule>Do not claim MELPA availability before the recipe is merged.</rule>
</rules>

<rules priority="standard">
  <rule>Prefer `defvar-local`, imperative command docstrings, `https` URLs, and consistent GPL boilerplate.</rule>
  <rule>Keep test files out of the package feature graph.</rule>
  <rule>Separate blocking review findings from deferrable design calls; record why anything is deferred.</rule>
</rules>

<related_skills>
  <skill name="emacs-ecosystem">Emacs Lisp language, package.el, and editor-integration foundations</skill>
  <skill name="lisp-macro">Macro-heavy source that must still satisfy package-lint prefix and docstring checks</skill>
  <skill name="technical-writing">README, Commentary, and changelog wording during the pre-acceptance window</skill>
  <skill name="context7-usage">Fetch current package-lint / package.el documentation when a check is ambiguous</skill>
</related_skills>

<related_agents>
  <agent name="quality-assurance">Review package headers, docstrings, and lint findings against this guidance</agent>
  <agent name="explore">Locate shipped-vs-generated files and recipe/Makefile drift across the repo</agent>
  <agent name="docs">Align README/changelog availability claims with actual acceptance state</agent>
</related_agents>
