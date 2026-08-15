---
name: quality-tools
description: Running quality checks so their output is trustworthy, plus cohesion-raising refactor operations, project-local policy gates, supply-chain pinning, and the honesty rules for a scored review.
version: 3.0.0
---

The invocation a project's own configuration declares always wins over anything here. The tool names below are
a fallback for when it does not settle the question — everything after them is what the tools themselves cannot
tell you.

## Invocations

| Language | Format | Lint | Types |
|---|---|---|---|
| JS/TS | `prettier --write`, or `biome format` | `eslint`, or `biome lint` | `tsc --noEmit` |
| JS/TS, one pass | `biome check --fix` — lint and format together, far faster; prefer it when the ESLint plugin ecosystem is not needed | | |
| Go | `gofmt -l` | `go vet ./...`, `staticcheck ./...` | |
| Rust | `cargo fmt --check` | `cargo clippy` | `cargo check` |
| Nix | `nixfmt` | `statix check` | `nix flake check` |
| Python | `ruff format` | `ruff check --fix` | `mypy` |
| PHP | `php-cs-fixer fix` | `phpstan analyse` | |
| Haskell | `fourmolu -i` | `hlint` | `cabal build --ghc-options="-Wall -Werror"` |
| C/C++ | `clang-format -i` | `clang-tidy` | |
| Swift | `swift-format format -i` | `swiftlint` | |

ESLint 10 is flat-config only — `eslint.config.js`/`.ts` with `defineConfig()` and `globalIgnores()`; the
eslintrc format is fully removed, and config resolves from **each linted file's directory, not the working
directory.**

Run types first, then lint, then formatter — the type checker gives the fastest useful feedback and its errors
often explain the lint findings. Exit 0 means clean, 1 means issues found, 2 usually means the tool itself
failed to run, and the third is the one to check for before reading the second as a pass.

Auto-fix formatting and import order freely; review anything that changes logic; never auto-apply a fix to a
security finding. Verify with tests after any fix pass, and keep formatting changes in their own commit so the
logic diff stays readable.

## Reading diff output mechanically

**Neutralize any configured external diff driver before parsing a diff.** A repo- or user-level
`diff.external` / `diff.tool` setting — difftastic, delta, and similar — makes `git diff`, `git show`, and
`git log -p` emit syntax-highlighted, structurally reformatted text rather than a parseable unified diff. Pass
`--no-ext-diff`.

The failure is silent: the command exits zero and the reader draws wrong conclusions from decorated output
instead of hitting an error. **Check this before concluding a diff is empty or a change is missing** — a
setting made globally for the machine applies to every repository, not just the one that documents it.

## Coverage percentages

An aggregate percentage is computed over the files that appear in the report, so **it says nothing about a file
dropped from it.** A production file that failed to instrument, or was never loaded by the run, contributes
nothing to the denominator — and a hundred percent over nine of ten files reads exactly like a hundred percent
over ten.

Gate on a declared source manifest, not the total alone. Compare the report's normalized filenames against the
list of files that are supposed to be covered, and reject the run when a declared file is missing, when a row
is malformed, or when a row's total is zero. **A zero-total row means the file was seen but never executed —
the same news as its absence, and just as easy to overlook.**

This is the coverage form of the empty-selector problem: the check passes because it found nothing to disagree
with. See [testing-patterns](../testing-patterns/SKILL.md) for the suite-level version.

## Refactoring operations

Language-neutral, behavior-preserving moves that raise cohesion. **Apply the smallest one that addresses the
finding**, and run the targeted tests and type check after each move.

**View-data extraction** — move the branching that decides *what to show* out of a view into a pure selector,
leaving the view render-only. Use it when a component mixes decision logic with rendering and so cannot be
tested without rendering it. A component is legitimately render-only only once such a selector exists.

**Static data / logic split** — move inert values out of a behavior module into a sibling data module. The
boundary rule: a stable constant, a display limit or threshold, an immutable marker or copy string, or a
lightweight exported contract moves; anything that formats, parses, touches the filesystem or network,
coordinates async work, or changes session state stays. Different axis from view-data extraction — this one
moves inert values and applies equally to server code with no view at all.

**Wiring / implementation split** — separate state, transport, and lifecycle from the actual computation by
extracting the computation into a focused helper. Use it when a hook, handler, or service grew because it does
both.

**Barrel removal** — replace a thin re-export module with direct imports, then delete it. Three caveats: a
package-root index the manifest names as the entry point is a config-bound public API, not a shim — retarget
the manifest first; **a file that defines anything of its own is not a barrel** even when it looks like one, so
split those definitions out and move consumers instead; and keep a canonical public aggregation point until
every caller has migrated off it.

**Single-consumer aggregator inlining** — inline a wrapper whose only job is bundling already-local state for
exactly one caller. Unlike barrel removal the target *composes* rather than re-exports, so a search for pure
re-export files will not find it. **Standing prohibition:** do not reintroduce a feature-root composition layer
during a later split unless it carries logic beyond forwarding. This wrapper tends to come back, because adding
one is the reflex when splitting a module.

**Over-abstraction reversal** — collapse a seam that does not pay for itself. Two tests: does the seam carry a
*decision of its own*, or does it only relocate a step while costing a name, a call, and its own unit tests?
And do the candidates share *semantics* or only *structure* — two handlers with matching shape but differing
validation, decoding, side effects, and limits are a recurring false positive for DRY. This direction needs to
be in the catalog: every other operation extracts, so without a reversal an over-eager split has no documented
remedy.

### Stop rules

A catalog of extraction operations invites continuous splitting, so it needs criteria for declaring a module
finished.

- **A module that is already thin orchestration over dedicated helpers is done.** Extracting further adds
  indirection without reducing complexity.
- Target modules holding concrete branching or mutable state. Do not sweep a subtree for candidates by
  structure alone.
- Split further only when a concrete bug or a genuinely new responsibility appears, never on the general
  principle that smaller is better.
- Apply coverage goals to the slices actually refactored, not repo-wide as an undifferentiated target.

**Overrun signals:** a wrapper deleted, reintroduced, and deleted again; a data module created to hold two
constants.

When an extraction creates a new boundary module, **add a direct spec for that module.** Relying on the public
surface alone leaves the new seam exercised only incidentally, so coverage stays green while the boundary
itself is untested.

## Project-local policy gates

Checks enforcing a rule the off-the-shelf tools do not know about. Unlike the catalog above, you author and
maintain these, so their failure modes are yours too.

**A mechanical migration does not finish when the last file is edited. It finishes when the old idiom becomes
impossible to reintroduce.** Ship the check as a test inside the normal suite, not a separate tool someone has
to remember. Scan **emitted output** — format strings, generated text — rather than whole-file text: a
whole-file scan fails on comments and documentation, which is how a change that keeps runtime logic intact and
merely renames surrounding prose ends up red. Make it table-driven, one call site per migrated file, so
coverage gaps are visible by inspection. Ship a narrower variant for legitimate exceptions, so the escape hatch
is explicit rather than achieved by weakening the rule for everyone.

**Home-grown layering and purity checkers are almost always regex over source text**, so they match
identifiers, comments, and string literals indistinguishably from real API references. Treat a hit as evidence
to investigate, not proof of a violation. Inside directories governed by such a check, avoid naming local
identifiers after the forbidden APIs — a slightly different local name costs far less than a permanently noisy
gate reviewers learn to ignore.

A convention is not adopted until a machine gate enforces it, and a gate is only worth having while its
precision keeps it trusted. **Those two pull in opposite directions when the check is textual.**

## Supply chain

**Pin every external reference to an immutable identifier** — a full commit hash or exact version — and keep
the human-readable tag in an adjacent comment. A floating reference (a moving tag, an `@latest` alias, an
unversioned path) can change what the build produces **without any repository diff**, which is what makes it
worth a standing rule rather than case-by-case judgement.

Where the reference is data, assert the pinned form in a test — matching asset URLs against an exact-version
pattern, for instance. That turns an upgrade into a deliberate reviewable edit instead of silent drift.

Keep automation credentials at the narrowest scope by default, widening only where a specific job demonstrates
the need. Scan the **working tree**, not only history, so an unstaged credential is caught before it is
committed. Validate CI workflow syntax statically, and audit workflows for supply-chain weaknesses
specifically — unpinned references, over-broad permissions, untrusted input reaching a shell.

## Scored reviews

Report each dimension separately as a one-line observable status — what was checked, what it returned, what was
not exercised — **so weak areas stay visible rather than averaged into one number.** Separate findings into
critical (must fix before release) and quick wins (high impact, low effort), and sequence remediation into
phases so the report reads as a rollout plan rather than a flat list.

Honesty rules:

- State the analysis's basis and its limits. **An architectural or static review is not runtime measurement.**
  Never present estimated improvements as measured results.
- Tag the basis with its evidence tier, and name what was not exercised: real workloads, low-resource systems,
  actual profiling.
- **A score is valid only for the tree state it was computed against, so record that state.** A stored
  scorecard outlives the code it scored and reads as current evidence to whoever finds it next.
- When an obsolete scorecard turns up, invalidate it explicitly rather than carrying it forward: say it must
  not be used for current prioritization, and mark which items are resolved or superseded. **Superseding a
  score requires re-measurement** — unbenchmarked estimates do not become evidence by ageing.

### Label integrity

Applies to any report, dashboard, or evaluation table, not only review output.

**Every label must be derived from the same key it displays.** A column headed with one qualifier while reading
a differently-qualified source key produces a confident wrong number — worse than a missing one, because
downstream decisions rely on it. For each label in a template, confirm the key it reads carries the same
qualifier. **This defect survives review because the code is locally correct and the label is locally
reasonable; only the pairing is wrong.**

## Related

- [testing-patterns](../testing-patterns/SKILL.md) — running tests after a fix pass, and the suite-level empty-selector problem
- [test-integrity](../test-integrity/SKILL.md) — whether the green these tools report proves anything
- [git-ecosystem](../git-ecosystem/SKILL.md) — the broader Git configuration surface behind `--no-ext-diff`
- [workflow-patterns](../workflow-patterns/SKILL.md) — a convention is unadopted until a gate enforces it
- [execution-workflow](../execution-workflow/SKILL.md) — where these checks sit in the definition of done
