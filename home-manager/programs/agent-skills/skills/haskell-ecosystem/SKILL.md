---
name: haskell-ecosystem
description: "Use when working with Haskell projects, 'cabal build', 'stack build', 'cabal.project', 'stack.yaml', 'ghc', or Haskell language patterns. Provides comprehensive Haskell patterns for type system, monads, Cabal/Stack build systems, testing, and type-level programming."
---

Comprehensive patterns for Haskell language, GHC toolchain, Cabal/Stack build systems, and type-level programming.

## Core Concepts

- **Purity**: Functions have no side effects; same input always produces same output
- **Laziness**: Expressions evaluated only when needed; enables infinite data structures
- **Type inference**: Hindley-Milner system; explicit signatures recommended for top-level definitions
- **Monads**: Compose computations with effects (IO, Maybe, Either, State, Reader, Writer)
- **Type classes**: Ad-hoc polymorphism (Eq, Ord, Show, Functor, Applicative, Monad)

## Development Workflow

1. **Analyze**: Check `.cabal`/`package.yaml`, review types and type classes, identify monad requirements
2. **Implement**: Design with types first, use Functor/Applicative/Monad, handle errors with Maybe/Either/ExceptT
3. **Validate**: Run `cabal build`, then `hlint src/`, then `cabal test`, then `fourmolu --check`

## Type System

### Algebraic Data Types

```haskell
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data Person = Person { name :: String, age :: Int }
```

### Key Type Classes

`Eq` (==), `Ord` (compare), `Show` (show), `Functor` (fmap), `Applicative` (<*>), `Monad` (>>=), `Foldable` (foldr), `Traversable` (traverse), `Semigroup` (<>), `Monoid` (mempty)

### GADTs

```haskell
{-# LANGUAGE GADTs #-}
data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
```

### Type Families and DataKinds

```haskell
{-# LANGUAGE TypeFamilies #-}
type family Element c where
  Element [a]     = a
  Element (Set a) = a
  Element Text    = Char

{-# LANGUAGE DataKinds, KindSignatures #-}
data Vec (n :: Nat) a where
  VNil  :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a
```

## Monad Transformers

```haskell
-- MTL-style (preferred)
doSomething :: (MonadReader Config m, MonadState AppState m, MonadError AppError m) => m Result
doSomething = do
  cfg <- ask
  st <- get
  when (invalid cfg) $ throwError InvalidConfig
  pure (compute cfg st)

-- Concrete stack
type App a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a
```

**Common transformers**: `ReaderT` (ask, local), `StateT` (get, put), `ExceptT` (throwError), `WriterT` (tell), `MaybeT`

## Error Handling

```haskell
-- Maybe for optional values
findUser :: UserId -> Maybe User

-- Either for errors with information
parseConfig :: Text -> Either ParseError Config

-- ExceptT in monadic contexts
loadUser :: MonadError AppError m => UserId -> m User
loadUser uid = do
  mUser <- findUser uid
  case mUser of
    Nothing -> throwError NotFound
    Just u  -> pure u
```

## Common Patterns

```haskell
-- Newtype for type safety (zero-cost)
newtype UserId = UserId { unUserId :: Int } deriving (Eq, Ord, Show)

-- Smart constructors
module Email (Email, mkEmail, unEmail) where
newtype Email = Email Text
mkEmail :: Text -> Maybe Email
mkEmail t | isValidEmail t = Just (Email t)
          | otherwise      = Nothing
```

## Optics (lens library)

```haskell
import Control.Lens
data Person = Person { _name :: String, _age :: Int }
makeLenses ''Person

getName p = p ^. name        -- view
setName n p = p & name .~ n  -- set
modifyAge f p = p & age %~ f -- modify
```

**Operators**: `^.` (view), `.~` (set), `%~` (modify), `^?` (preview), `^..` (toListOf)

## Cabal

```
cabal-version: 3.0
name:          my-project
version:       0.1.0.0

library
  exposed-modules:  MyProject
  build-depends:    base ^>=4.18, text ^>=2.0
  hs-source-dirs:   src
  default-language: GHC2021
```

**Commands**: `cabal build`, `cabal test`, `cabal run`, `cabal repl`, `cabal haddock`, `cabal freeze`

## Stack

```yaml
resolver: lts-22.0
packages:
  - .
extra-deps:
  - some-package-1.0.0
```

**Commands**: `stack build`, `stack test`, `stack run`, `stack ghci`, `stack clean`

**Cabal vs Stack**: Stack for reproducible builds (Stackage LTS); Cabal for Hackage publishing and complex overrides; Stack for newcomers; Cabal with haskell.nix for Nix integration.

## Testing

```haskell
-- HSpec (BDD-style)
main = hspec $ do
  describe "Calculator" $ do
    it "adds two numbers" $
      add 1 2 `shouldBe` 3

-- QuickCheck (property-based)
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

-- Combined
it "is its own inverse" $ property $
  \xs -> reverse (reverse xs) == (xs :: [Int])
```

**Matchers**: `shouldBe`, `shouldSatisfy`, `shouldReturn`, `shouldThrow`, `shouldContain`

## GHC Toolchain

- **GHC 9.12+**: Current version; use `GHC2021` or `GHC2024` as default language
- **HLS**: IDE support (completion, hover types, go-to-definition, hlint diagnostics)
- **Formatters**: `fourmolu` (opinionated, configurable), `ormolu` (minimal config)
- **Linters**: `hlint` (idiomatic suggestions), `stan` (static analysis), `weeder` (dead code)

## Anti-Patterns

- **Partial functions**: Avoid `head`, `tail`, `fromJust`, `read`; use safe alternatives or pattern matching
- **String type**: Use `Text` or `ByteString` for performance
- **Lazy IO**: Use strict IO or streaming (conduit, pipes) in production
- **Orphan instances**: Use newtype wrappers or define instances in appropriate modules

## Critical Rules

- Run `hlint` before committing; address all suggestions
- Avoid partial functions; use safe alternatives
- Use types to encode invariants; make illegal states unrepresentable
- Prefer `Text` over `String` for text processing

## Context7 Integration

Libraries: Servant (`/haskell-servant/servant`), HSpec, Hedgehog, Aeson, Optics, Criterion

```
get-library-docs context7CompatibleLibraryID="/haskell-servant/servant" topic="server"
```

## Related Skills

- `serena-usage`: Navigate type class hierarchies and module structure
- `nix-ecosystem`: haskell.nix integration and nixpkgs Haskell infrastructure
- `investigation-patterns`: Debug type errors, missing instances, performance issues
