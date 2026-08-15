---
name: haskell-ecosystem
description: Use for Haskell projects, covering cabal.project, stack.yaml, ghc, cabal or stack build/test/run, and Haskell language patterns.
version: 3.0.0
---

Patterns for the GHC toolchain and Cabal/Stack build hazards, version-specific language behavior, and the traps
that break Haskell code silently rather than at compile time.

## Type-level programming

GADTs, type families, and DataKinds refine what the compiler can prove; reach for them only when a plain ADT
plus a smart constructor cannot express the invariant — each one narrows what compiles, so an overused one turns
ordinary refactors into type-level puzzles for the next reader.

```haskell
{-# LANGUAGE GADTs #-}
data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
```

**Existentials erase type information at the point they're packed**, so prefer an explicit sum type
(`data Item = ItemInt Int | ItemText Text`) over `data AnyShow = forall a. Show a => AnyShow a` for
heterogeneous collections — the sum type keeps pattern matching exhaustive and GHC's -Wincomplete-patterns
useful; the existential does not.

## Effect systems and monad transformers

mtl-style constraints (`MonadReader`, `MonadState`, `MonadError`) still work but **each transformer layer wraps
the next in a newtype**, and every `lift` through that wrapping costs allocation. `effectful` avoids the
wrapping and gives sharper error messages — prefer it for new projects where performance matters; keep mtl in an
existing codebase rather than migrating wholesale. `bluefin` trades the type-level effect list for
handle-passing; treat it as experimental rather than a default choice.

`ListT` from `transformers` has broken semantics for non-determinism (it does not commute with the inner monad
correctly) — use `list-t` or `logict` instead.

## Linear types

`LinearTypes` has been stable since GHC 9.0 and enforces exactly-once use at the type level (`a %1 -> b`,
`Handle %1 -> IO a`). It matters for resource-safe APIs and hot paths; the annotation overhead is not worth
paying in ordinary application code.

## Records: NoFieldSelectors + OverloadedRecordDot

Since GHC 9.2 this is the standard replacing prefixed field names (`configHost`, `personName`).
**`NoFieldSelectors` suppresses the top-level accessor functions that would otherwise clash** across records
sharing a field name; `OverloadedRecordDot` restores access via `cfg.host`. Mixing the old prefixed convention
into a codebase that has already adopted this pattern reintroduces the exact clash the extension exists to
avoid.

## Silent traps

- **Partial functions** (`head`, `tail`, `fromJust`, `read`) crash at a call site the type signature gives no
  warning about, because the type checker cannot see the missing case — pattern-match or use
  `listToMaybe`/`headMay` instead.
- **`String` (`[Char]`)** is a linked list of boxed `Char` cons cells, so every character costs a heap
  allocation; use `Text`/`ByteString` once volume matters.
- **Lazy IO** (`readFile`, `getContents`) ties the file handle's lifetime to how the caller happens to force the
  result, not to where the code appears to close it — the handle can stay open (or the read can throw) well
  past the function that looks like it owns the resource. Use strict IO or streaming (`conduit`, `pipes`,
  `streaming`) in production.
- **Orphan instances** (defined outside the module of both the class and the type) compile cleanly in isolation
  and conflict silently the instant two libraries define the same orphan — GHC accepts one without a
  disambiguation error unless `-Wall`'s `-Worphans` catches it first.
- **Unbounded dependencies** (a bare `containers` with no version constraint) build today and break on the next
  `cabal update`/`stack build` the moment a future major release changes an API in use, with no local diff to
  explain why.

## Cabal

`cabal.project` scope hazards:

```
packages: .
          ./subpackage
optional-packages: ../local-dependency   -- silently skipped if missing; packages: fails hard instead
allow-newer: base                        -- overrides ALL bounds mentioning base, not just the direct dependency
```

Version bounds use `^>=` (PVP-compatible caret: pins the major version) — `base ^>=4.22` means `4.22.x.x` and
maps to GHC 9.14; `base ^>=4.21` maps to GHC 9.12.

Commands:
```
cabal build all      # builds every target, not just the default component
cabal freeze          # locks the resolved plan into cabal.project.freeze
cabal gen-bounds       # proposes PVP bounds from the resolved plan
cabal outdated
```

cabal-install 3.14+.

## Stack

`stack.yaml` pins a Stackage LTS resolver; `extra-deps` adds packages the resolver excludes, including Git pins
(`github: owner/repo` + `commit:`). **`ghc-options: "$locals": -Wall -Werror` applies only to packages under
`packages:`, not to `extra-deps`** — a warning that would fail the build in local code passes silently in a
vendored extra-dep with the identical warning.

`package.yaml` (hpack) regenerates the `.cabal` file on every build; editing the generated `.cabal` directly is
overwritten on the next `stack build` with no warning that the edit was lost.

Cabal vs Stack: Stack for Stackage-pinned reproducibility or onboarding simplicity; Cabal (`cabal.project`) for
Hackage publishing or fine-grained dependency overrides; `haskell.nix`/nixpkgs for Nix-integrated builds under
either.

## Toolchain

GHC 9.14.1 is the latest major and the first release under the LTS policy (minimum two years of bugfix-only
support, no backported features); GHC 9.12.4 is the latest 9.12 bugfix. Majors release twice a year.

`default-language: GHC2024` (available since GHC 9.10) extends GHC2021 with `ExplicitNamespaces`, `TypeData`,
`MonoLocalBinds`, and others. **GHC's own compiler default is still GHC2021**, not GHC2024 — set the language
edition explicitly per package rather than relying on the compiler default.

Formatters: `fourmolu` (ormolu fork, configured via `fourmolu.yaml`) is the recommended default; `ormolu` for
zero-configuration formatting; `stylish-haskell` is legacy, superseded by fourmolu.

Linters: `hlint src/` for idiomatic suggestions (per-rule severity in `.hlint.yaml`); `weeder` for dead code;
`stan` for broader static analysis.

## Testing

`cabal test` / `stack test` run whichever `test-suite`/`tests:` stanza is declared — a stanza that exists but is
never referenced by `packages:`/`tests: True` builds nothing and reports no failure. Use QuickCheck or Hedgehog
for properties, HSpec for example-based behavior, Tasty when one runner must unify HUnit, QuickCheck, and other
providers. Hedgehog's generators shrink automatically via integrated `Range`s; QuickCheck's `Arbitrary`
instances need a manually written `shrink` for anything beyond the types with built-in instances, or a failing
case reports itself unshrunk and much harder to read.

## Related

- [context7-usage](../context7-usage/SKILL.md) — fetch current Hackage/library docs. `mtl`, `transformers`,
  `lens`, and `QuickCheck` are not indexed in Context7; use hackage.haskell.org/package/{name} directly for
  those instead.
- [investigation-patterns](../investigation-patterns/SKILL.md) — debug type errors, missing instances, and
  performance issues.
- [nix-ecosystem](../nix-ecosystem/SKILL.md) — haskell.nix integration and nixpkgs Haskell infrastructure.
- [serena-usage](../serena-usage/SKILL.md) — navigate type class hierarchies and module structure by symbol
  rather than by text search.
