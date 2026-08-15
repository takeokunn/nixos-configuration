---
name: typescript-ecosystem
description: Use for TypeScript language patterns, tsconfig, project references, generics, and monorepo build graphs. Covers rootDir and package-boundary errors, typecheck versus build graph separation, and type-modeling patterns such as discriminated unions.
version: 3.0.0
---

TypeScript patterns where correct-looking code type-checks and is wrong, plus the compiler and
monorepo behaviors that are not discoverable from the type system alone.

## tsconfig

Baseline for the current Node.js LTS/current split — `module`/`moduleResolution: nodenext`, `strict: true`,
declaration/source maps on, `rootDir`/`outDir` split:

```json
{
  "compilerOptions": {
    "target": "ES2024", // ES2025 on Node.js 26 current
    "lib": ["ES2024"],  // ES2025 on Node.js 26 current
    "module": "nodenext",
    "moduleResolution": "nodenext",
    "strict": true,
    "verbatimModuleSyntax": true,
    "skipLibCheck": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "outDir": "./dist",
    "rootDir": "./src"
  },
  "include": ["src"],
  "exclude": ["node_modules", "dist"]
}
```

Node.js 24 is Active LTS (target ES2024); Node.js 26 is current (target ES2025).
`noUncheckedIndexedAccess` is not implied by `strict` and is worth enabling separately — it adds `undefined`
to every index-signature read, which is where most out-of-bounds bugs hide.

**`baseUrl` is deprecated in TS 6.x and scheduled for removal in TS 7.x** — prefer `paths` without it.
`moduleResolution: "node"` (alias `node10`) is removed in TS 6.0; use `nodenext` or `bundler`.

### Case-variant sibling filenames resolve to the same module

macOS and Windows filesystems are case-insensitive but case-preserving. Two modules whose basenames differ
only in case are distinct files to the editor and to git, but **the same path to the module resolver** — a
helper placed beside a component with a case-variant name can resolve to the component and import itself.

```ts
// Same directory, case-insensitive filesystem:
//   NotificationBanner.tsx   (component)
//   notificationBanner.ts    (helper)  ← ./notificationBanner may resolve to the .tsx
//
// Safe: give the helper a distinct name, e.g. notification-banner-format.ts
```

The failure presents as a circular import or an undefined export, never as "file not found" — it reads like
a bug in the module rather than in its name. It is also platform-dependent: it can pass on a case-sensitive
CI runner and fail on developer machines, or the reverse.

### Separate typecheck and build graphs

Typechecking and building want different file sets: the typecheck must include tests (otherwise test code is
never checked), and the production build must exclude them (otherwise test types ship). Serving both from one
config forces a choice between the two — run two configs instead, a test-inclusive one for `--noEmit` and a
production-only one for emit.

```json
// tsconfig.json — typecheck graph: sources + tests, no emit
{ "compilerOptions": { "noEmit": true, "incremental": true, "tsBuildInfoFile": "./.tsbuildinfo.check" },
  "include": ["src", "test"] }

// tsconfig.build.json — build graph: sources only
{ "extends": "./tsconfig.json",
  "compilerOptions": { "noEmit": false, "outDir": "./dist", "tsBuildInfoFile": "./.tsbuildinfo.build" },
  "include": ["src"] }
```

**Give each config its own `tsBuildInfoFile`.** Two configs sharing one incremental state file silently
clobber each other, so every run is a cold rebuild — no error, no warning, just mysteriously slow builds. A
single hardcoded `.tsbuildinfo` is exactly the configuration that causes this.

### Respect the package boundary (TS6059 / TS6307)

In a monorepo, `TS6059` ("file is not under rootDir") and `TS6307` ("file is not listed within the file
list") mean the compilation has reached outside the package root. The instinctive fix — widening `rootDir` —
dissolves the boundary that produced the error and turns one package's build into a build of its neighbours.
Fix the entrypoints instead:

- Importing a sibling package's subpath implementation file (`pkg/src/internal/x`) instead of its declared
  public entrypoint.
- Importing another package's test tree (`pkg/test/helpers`) from this package's tests.
- A test-only path alias that resolves outside the package root, dragging those files into the program.

Route every cross-package import through the sibling's public entrypoint and wire packages together with
project `references`. If a test helper is needed in two packages, copy the small helper locally or promote it
to a public export — never reach into a neighbour's test tree.

## Monorepo version skew

In a package graph, the code that runs is decided by where a value is **constructed** and by what the
installer actually resolved — neither of which is visible in the consumer's dependency list. A graph that
typechecks cleanly can still run two copies of the same library, or an older one than the manifest suggests.
These bugs are expensive because every artifact you'd normally consult says the change was applied.

**Runtime version follows the constructor.** A package that constructs a stateful service must itself depend
on the exact API version the host expects. Adding a newer direct dependency to the host does not upgrade an
object built inside a producer package that still resolves the older copy — the host holds a new type and an
old implementation. The same shape appears with data tables: the host is pinned to the new table while the
producer package that reads it resolves the previous one. When upgrading a shared API, bump it in every
package that constructs values of that API, not only the package that consumes them, and verify against the
installed tree (the package manager's `why`/`ls` output), not the manifests. The type-level variant is
equally common: two installed copies of a nominally identical closed union are distinct types, so literals
that share names produce incompatible public unions and an error message that reads as unrelated.

**Source-aliased siblings need full public exports.** Aliasing sibling packages to their source is the
standard monorepo fast-feedback setup, and its standard failure is a green typecheck with a runtime
explosion: the dev-time resolver reaches a symbol through source that the published entrypoint does not
export. Every symbol the running application touches must be exported from the sibling's public entrypoint —
a transitive dependency's internal export may typecheck in isolation and be unreachable once real resolution
rules apply. Alias the package that *owns* a service as well as its consumer; a half-aliased graph lets stale
nested declarations resolve alongside the aliased source, producing structurally incompatible versions of the
same service type at the composition boundary.

**Vocabulary changes are vertical and bottom-up.** Adding a member to a closed vocabulary (a shared union of
string literals, an id space, a status set) is not a local edit. Add the canonical literal in the package
that owns the vocabulary, release it, update mirrored vocabularies and rules in intermediate packages,
release those, and only then pin and integrate at the host. Any other order produces a window where the type
and the runtime disagree about what values exist. Never mirror a closed union downstream, including in
runtime guards — import both the type and its guard from the owning package, so a newly registered member is
immediately valid everywhere. Note: adding a member to an exported closed union is an additive public API
change (semver MINOR); while a package is on 0.x, that maps to a PATCH bump, which routinely surprises people
reading the version alone.

## Native TypeScript execution

Node.js 24+ runs `.ts` files directly (`node src/index.ts`) with no `tsx`/`ts-node`; Node.js 22.6+ needs
`--experimental-strip-types`. Node strips type annotations at runtime, so code must be **erasable syntax
only** — no `const enum`, namespaces with runtime code, or legacy parameter properties. Enable
`erasableSyntaxOnly` in tsconfig to catch these at compile time instead of at Node's load-time throw.

## Type patterns

### Branded types

Nominal typing via intersection with a unique-symbol brand prevents mixing structurally identical primitives
(`UserId` vs `OrderId`):

```ts
type UserId = string & { readonly __brand: unique symbol };
type OrderId = string & { readonly __brand: unique symbol };

function createUserId(id: string): UserId {
  return id as UserId;
}
```

### Exhaustive registries

When one logical variant must be registered in several parallel places — a renderer table, a capability
list, a serializer map — a checklist in a document decays and an exhaustive mapped type does not. Type each
registry as `Record<Union, T>` so that adding a member to the union turns every unupdated registry into a
compile error instead of a runtime hole.

```ts
type Shape = "circle" | "square" | "triangle";

// Total: omitting a member is a compile error.
const renderers: Record<Shape, Renderer> = {
  circle: renderCircle,
  square: renderSquare,
  triangle: renderTriangle,
};

// Partial: a missing member is `undefined` at runtime, and nothing complains.
// const renderers: Partial<Record<Shape, Renderer>> = { circle: renderCircle };
```

Prefer a total `Record<Union, T>` over a partial map or a lookup function with a fallback — the fallback is
what converts a missing registration from a build failure into a silent default at runtime. The same
reasoning favors an exhaustive `switch` with a `never`-typed default assertion over a `default` arm that
quietly absorbs new members.

**Ordinal position as persisted encoding.** When a union member's ordinal position *is* the persisted
encoding (an index into a codec table, a wire tag derived from array order), inserting a member is a data
migration, not an edit — every previously stored value after the insertion point now decodes as its
neighbour. Append only. Any parallel array keyed by that ordinal must be extended at the same index in the
same change; to remove a member, retire the slot rather than compacting the list. List order also becomes
observable wherever selection uses modulo or round-robin over the list, so even a pure append can perturb
behavior and tests that depended on the cycle.

### Discriminated unions consumed by generic machinery

A discriminated union consumed by generic machinery — a reducer, an event bus, a stream transition
applicator — should carry its payload in a field named for the payload's **structural role**, not its domain
meaning. Naming the terminal payload after what it happens to contain in one variant forces every generic
consumer to branch on the discriminant it was written specifically to avoid branching on.

```ts
// Forces ad-hoc branching in every generic consumer:
// | { kind: "done"; outputPath: string }
// | { kind: "done"; summary: Summary }

// Uniform: the applicator forwards `value` for either stream.
type Transition<T> =
  | { kind: "chunk"; text: string }
  | { kind: "done"; value: T }
  | { kind: "error"; error: Error };
```

Domain-meaningful names still belong inside the payload type — the rule is about the field the generic layer
reads, not about erasing meaning from the data.

### Schema as single source of truth

Define a value's shape once, as a runtime schema, and derive everything else from it. A schema library (Zod,
Valibot, ArkType) produces both a runtime validator and the static type via inference, so the type and the
validation can never drift apart. Never hand-maintain a separate interface alongside a validator.

```ts
const UserSchema = z.object({
  id: z.string().uuid(),
  email: z.string().email(),
});
type User = z.infer<typeof UserSchema>; // stays in lockstep with the validator
```

Attach descriptions to schema fields so the same definition powers validation, static types, and generated
artifacts (OpenAPI, JSON Schema, form metadata, LLM tool schemas) — one annotated schema replaces separate
docs. When one endpoint accepts multiple input encodings, validate each branch against its own schema and
normalize all branches into a single validated result shape, so downstream code never re-inspects which
encoding arrived.

**Absent is valid, malformed is fatal.** "Optional" is ambiguous between absent and present-but-invalid, and
parsers routinely collapse the two by omitting whatever fails to parse. That lenient reading turns a
producer's bug into silent data loss: the field disappears, the object still validates, and nothing
downstream can tell that anything was dropped. An optional field that is present with the wrong type must
fail the parse, not be dropped. This matters most in list processing: silently skipping malformed items
yields a plausible-looking short result indistinguishable from a genuinely short one, so a malformed item
should fail the whole read rather than shrink it. Where partial results genuinely are acceptable, return the
rejected items alongside the accepted ones so the caller can decide — the rule is against dropping them
invisibly, not against tolerance itself.

## Resource management

`using`/`await using` (TC39 Stage 3, TS 5.2+) call `Symbol.dispose`/`Symbol.asyncDispose` automatically at
scope exit; requires `"lib": ["ESNext.Disposable"]` or target ES2024+. Use `DisposableStack` for managing
multiple resources.

### Guarded subscription

Subscribing to an event source has three races that the obvious code loses. Install the release handle
**before** subscribing, make release idempotent, and let only the currently active release clear the shared
handle.

```ts
// BAD: a source that emits synchronously during subscribe() fires before
// `release` has been assigned, so the handler runs with no way to tear down.
// const release = source.subscribe(handler);

// GOOD: the handle exists before any event can arrive.
const ref: { current: (() => void) | null } = { current: null };

const start = () => {
  let unsubscribe: (() => void) | null = null;
  const release = () => {
    if (ref.current !== release) return; // a newer subscription owns the ref now
    ref.current = null;
    unsubscribe?.();                     // idempotent: safe to call twice
    unsubscribe = null;
  };
  ref.current = release;                 // installed first
  unsubscribe = source.subscribe(handler);
  if (ref.current !== release) unsubscribe(); // cancelled while we were subscribing
  return release;
};
```

- Race 1 — synchronous emission during `subscribe()`: the source fires before the assignment completes, so
  the handle must be installed first.
- Race 2 — cancellation during an awaited registration: teardown can run before registration resolves, so
  the late-registration branch must also release.
- Race 3 — superseded teardown: if any release may clear the shared handle, an old subscription's cleanup
  tears down its replacement. Guard on identity, as above — this is the subtle one and it is rarely written
  down.
- Stale events from a replaced subscription must be ignored by identity, not by a boolean flag that the
  replacement also sets.

### Browser storage: success vs durability

In a transactional browser store, request success and durability are different events. A wrapper that
resolves on the request's success callback reports "durable" when it only knows "accepted", and the write can
still be rolled back afterwards.

For IndexedDB, a write must resolve only after the enclosing readwrite `IDBTransaction` fires `complete`.
`IDBRequest.onsuccess` means the operation was accepted *within* the transaction; a later `abort` or `error`
on that transaction must surface to the caller as a storage failure. Reads are the asymmetric case: a
readonly request may resolve from `onsuccess`, because there is nothing to roll back.

```ts
// Writes: wait for the transaction, not the request.
const put = (store: IDBObjectStore, value: unknown) =>
  new Promise<void>((resolve, reject) => {
    store.put(value);
    store.transaction.oncomplete = () => resolve();
    store.transaction.onabort = () => reject(store.transaction.error);
    store.transaction.onerror = () => reject(store.transaction.error);
  });
```

The asymmetry is what makes a naive uniform wrapper wrong: applying request-level resolution to both reads
and writes looks consistent and reports false durability, while applying transaction-level resolution to
reads needlessly serializes them.

## Server/client boundary (React Router v7, Remix, Next.js)

A single codebase compiles for two targets. Server-only code — DB clients, secrets, `node:` modules — must
never reach the client bundle. Enforce this with an explicit boundary the bundler understands, not with the
hope that tree-shaking drops unused code; correctness must not depend on an optimization.

Name server-only modules with a `.server.ts` suffix (or a `.server/` directory) — framework bundlers treat
this as a hard boundary and exclude it from the browser build. Server-only route exports (`loader`, `action`,
`headers`) and imports of server-only packages are the other two recognized forms of server-only code. Do not
rely on tree-shaking for correctness: a value that must stay server-side belongs behind the `.server`
boundary even if it currently looks unused on the client.

A barrel (`index.ts`) that re-exports server implementations alongside client-safe types pulls the server
code into the client bundle the moment any client-reachable file does a value import from it. Forbid value
imports of server-mixed barrels from client-reachable code with a lint rule (`no-restricted-imports` with
`allowTypeImports`); permit type-only imports. Client code should import from safe subpaths (e.g.
`pkg/domain`, `pkg/application`) rather than the package root barrel.

On request handlers, accept an explicit allow-list of content types (`application/json`,
`multipart/form-data`, `application/x-www-form-urlencoded`) and reject the rest rather than auto-guessing.
Parse and validate each accepted branch, then normalize to one validated result. Prefer form-data-only
handling for in-page forms; add JSON support only where a non-browser/external caller genuinely needs it.

## Edge/isolate runtimes (Cloudflare Workers and similar V8-isolate platforms)

These are not Node.js: no synchronous filesystem, `import.meta.url` is undefined, and `process.env` is not
the configuration source. Code that assumes the Node module/file model fails at module-initialization time —
before any request is handled — which makes the failure look like a deploy error rather than a runtime one.

Do not read files or resolve paths at runtime in an isolate. Because `import.meta.url` is undefined, any path
derived from it (or any `createRequire(import.meta.url)` a bundler emits for CJS interop) throws at load.
Inline content at build time via static imports instead:

```ts
// Fails on isolates: resolves a path from import.meta.url at runtime.
// const text = readFileSync(new URL("./prompt.md", import.meta.url));

// Works: content is a static import, inlined at build time.
import { promptTemplates } from "./prompts.js";
```

If a dependency crashes at load with an undefined-path error, the usual cause is a bundler defaulting to a
Node platform target — target a web/neutral platform so no code depends on a runtime `import.meta.url`.

Isolates provide secrets/config through a per-request env binding, not `process.env` at module load. A
module-level singleton constructed from `process.env` reads empty values. Export factory functions that
accept config explicitly and construct per request:

```ts
// Fails on isolates: reads the key at module-init from process.env.
// export const client = makeClient(process.env.API_KEY);

// Works: factory receives the key from the request-time env binding.
export const createClient = (apiKey: string) => makeClient(apiKey);
// handler: const client = createClient(env.API_KEY);
```

## Auth and security patterns

Authentication boundaries fail open when a check is merely structural rather than cryptographic, or when
security-relevant state lives somewhere that does not survive scale-out. Treat every externally supplied
token and every cross-request nonce as adversarial.

- An ID token from an external identity provider must be **signature-verified** — fetch the provider's JWKS,
  verify the RS256/ES256 signature, and handle key id (`kid`) rotation — before trusting any claim. Parsing
  the payload and checking its structure is not verification; it accepts forged tokens.
- Store OAuth `state` (and similar one-time nonces) in a shared store (Redis/DB) and delete it on first use.
  A process-local `Map` breaks under multiple instances or a restart, and a state that cannot be reliably
  validated cannot protect against CSRF on the callback.
- Cookie-authenticated mutating routes need CSRF/Origin verification (403 on failure). Set JWTs in HTTPOnly
  cookies from the server; the client only navigates/redirects. Synchronize the JWT `exp` and the cookie
  `Max-Age` from one source so they cannot drift. Store only a hash (SHA-256) of refresh tokens and rotate
  them one-time (invalidate on use, issue a new one).

**One-generation rotation recovery window.** One-time rotation (invalidate the presented token, issue a new
one) has a liveness hole: if the response carrying the new token is lost — a dropped connection, a
backgrounded tab — the client is locked out holding a token the server has already consumed, and retrying
does not help. Retain the hash of the immediately previous token and accept it for exactly one recovery
rotation, which closes the hole without accepting unbounded replay. Store hashes only, keep exactly one
generation of history (not a list), and reissue on a previous-generation hit rather than treating it as an
attack — anything beyond one generation is a replay window, not a recovery window. The client half matters
too: hold a registration/bootstrap secret in memory only until acceptance, and clear it from session storage
at start-up, so a consumed secret cannot be replayed from disk. Be explicit about the residual edge this does
not fix: if the very first issuance is lost, the client never held a token to recover with, and only an
administrative path can re-provision it.

Do not let one login method's required environment (e.g. an embedded mini-app/in-app-webview integration)
throw at dependency construction and take down an unrelated login path (e.g. email). Validate a subsystem's
environment only when that subsystem is actually used.

## Hexagonal ports

Abstract every external dependency behind a port (an interface), with an adapter per environment — this is
what makes an application testable and portable across runtimes: the domain talks to ports, and only the
composition root knows which adapter is wired in. Non-obvious externalities — the clock, timers, randomness —
are dependencies too and should be ports.

Wrap timers/intervals in a `TimerPort` (e.g. `setInterval` that returns a cleanup function, plus lifecycle
observation) rather than calling the platform timer directly, so periodic and time-dependent behavior becomes
deterministic in tests via fake timers:

```ts
interface TimerPort {
  setInterval(fn: () => void, ms: number): () => void; // returns cleanup
}
// test: vi.useFakeTimers() drives the port deterministically
```

Provide ports through factory functions/constructor injection rather than module-level singletons — this
composes naturally with per-request injection on isolate runtimes and with mocking in tests, and keeps
intermediate types (raw parsed rows, DTOs) from leaking platform concerns into the domain.

## Tooling

ESLint 10 removed the eslintrc format entirely — flat config (`eslint.config.js`) only:

```js
import eslint from "@eslint/js";
import tseslint from "typescript-eslint";
import { defineConfig, globalIgnores } from "eslint/config";

export default defineConfig([
  globalIgnores(["dist/", "node_modules/"]),
  eslint.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  { languageOptions: { parserOptions: { projectService: true, tsconfigRootDir: import.meta.dirname } } },
]);
```

ESLint 10 locates `eslint.config.*` from each linted file's directory, not cwd — a config that works when
run from the repo root can behave differently invoked from a subpackage. `defineConfig()`/`globalIgnores()`
give type-safe config composition.

Biome is a unified linter/formatter (alternative to ESLint+Prettier) that is significantly faster; reach for
it when unified tooling and speed are priorities over the ESLint plugin ecosystem.

## TypeScript versions

TypeScript 6.0 is the current stable release and the final JavaScript-based compiler — a bridge release
between 5.x and 7.0, with no 6.1 planned. TypeScript 7.0 (`@typescript/native-preview`) is a Go-native
compiler rewrite with up to 10x performance improvement and ~8x project-load-time reduction in the language
service; it is near completion with nightly previews available and is the direction the language is moving.

## Related

- [serena-usage](../serena-usage/SKILL.md) — symbol-level navigation for type definitions and interfaces
- [context7-usage](../context7-usage/SKILL.md) — fetch current TypeScript compiler and tooling documentation
  instead of relying on stale training data
- [investigation-patterns](../investigation-patterns/SKILL.md) — debug type errors and investigate
  compilation issues
- [effect-ts](../effect-ts/SKILL.md) — Effect (Effect-TS) Service, Layer, Schema, and error-channel patterns
  built on top of TypeScript
- [state-transactions](../state-transactions/SKILL.md) — framework-neutral state ownership, atomicity, and
  durability-ordering rules; the browser-storage pattern above is the API mechanism, not the policy
- [trust-boundaries](../trust-boundaries/SKILL.md) — untrusted-input discipline that the schema and auth
  patterns above implement
- [quality-tools](../quality-tools/SKILL.md) — lint and refactoring gates that mechanically enforce the
  boundary rules above
