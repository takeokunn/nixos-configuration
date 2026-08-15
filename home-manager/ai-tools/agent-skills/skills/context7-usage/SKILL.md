---
name: context7-usage
description: Use for Context7 MCP documentation retrieval — current library docs, API signatures, version-specific behavior, or migration notes, to avoid stale API assumptions.
version: 3.0.0
---

Retrieve before claiming. **Any statement about an external library's API, configuration, or behavior comes
from a fetch, never from training-data memory** — the memory is confident, plausible, and dated.

Two tools: `resolve-library-id` turns a library name into a Context7 ID; `query-docs` fetches for a specific
topic. Skip the resolve step when the ID is already known.

## When

- The question is about a library's API, config options, or recommended usage.
- Code references an external dependency whose current behavior needs confirming.
- A version upgrade is in scope — check the migration notes before writing the change.
- Debugging "why doesn't this API work": verify against current docs before concluding it is a bug.
- Writing a nixpkgs derivation with a language builder. Context7 against `/nixos/nixpkgs` is the authoritative
  source for `buildGoModule`, `rustPlatform.buildRustPackage`, and their hash attributes.

For project-local evidence, use Serena or Read instead — Context7 answers what the library does, not what this
repository does.

## Query specifically

A broad topic string returns unfocused results and invites the model back onto its own recall.

```
query-docs libraryId="/microsoft/typescript" query="tsconfig moduleResolution nodenext"
query-docs libraryId="/nixos/nixpkgs"        query="buildGoModule vendorHash"
query-docs libraryId="/rust-lang/rust"       query="Rust edition 2021 to 2024 migration"
```

Name the version explicitly whenever behavior differs across versions — the answer to "does this API exist" is
version-dependent and a version-free query gets an answer for some version nobody chose.

## Known IDs

Use directly, without resolving.

| Library | ID |
|---|---|
| TypeScript | `/microsoft/typescript` |
| Nixpkgs / NixOS | `/nixos/nixpkgs` |
| Home Manager | `/nix-community/home-manager` |
| Go | `/golang/website`, `/golang/tools` |
| Rust | `/rust-lang/book`, `/rust-lang/rust` |
| Swift | `/apple/swift` |
| PHP | `/php/php-src` |
| GHC / Haskell | `/ghc/ghc` |

## Reading the result

Prefer official or primary documentation over derived material. State uncertainty plainly when the docs are
unavailable or ambiguous rather than filling the gap from recall — an unavailable doc is a gap to report, not a
prompt to guess.

Where Context7 and the local code disagree, that is a finding to investigate, not a conflict to resolve by
preferring one. And **project conventions outrank generic defaults from docs**: the documentation says what is
possible, the repository says what is done here.

## Related

- [fact-check](../fact-check/SKILL.md) — external verification for non-library claims
- [serena-usage](../serena-usage/SKILL.md) — project-local evidence, a complement rather than a substitute
- [nix-ecosystem](../nix-ecosystem/SKILL.md) — nixpkgs packaging, always paired with a Context7 fetch
