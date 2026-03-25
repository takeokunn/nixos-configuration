---
name: typescript-ecosystem
description: "Use when working with TypeScript projects, tsconfig.json, type definitions, generics, utility types, tsc, tsx, or ESLint for TypeScript. Provides comprehensive TypeScript ecosystem patterns for type system, configuration, and tooling integration."
---

Comprehensive patterns for TypeScript language, configuration, type system, and tooling integration. The agent should enable strict mode in all projects, prefer `unknown` over `any`, and run `tsc --noEmit` before committing.

## Critical Rules

- Enable `strict: true` in all TypeScript projects
- Never use `any` without documented justification; prefer `unknown`
- Run `tsc --noEmit` before committing to catch type errors
- Use `noUncheckedIndexedAccess` for safer array/object access

## Workflow

1. **Analyze** — Check tsconfig.json for project settings, review existing type patterns, identify type dependencies
2. **Implement** — Define types before implementation, use strict type checking features, follow project naming conventions
3. **Validate** — Run `tsc --noEmit` for type checking, check with ESLint for style issues, verify tests pass

## TSConfig

### Recommended Base (Node.js 22 LTS)

```json
{
  "compilerOptions": {
    "target": "ES2023",
    "lib": ["ES2023"],
    "module": "nodenext",
    "moduleResolution": "nodenext",
    "strict": true,
    "esModuleInterop": true,
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

For Node.js 24 (upcoming), use `ES2024` target/lib. Requires `"type": "module"` in package.json for ESM.

### Module Resolution

| Strategy | Use When |
|----------|----------|
| `nodenext` | Node.js ESM projects (recommended) |
| `bundler` | Vite, esbuild, webpack projects |

**Deprecation warnings:** `baseUrl` deprecated in TS 6.0, removed in TS 7.0. `moduleResolution: "node"` (alias `node10`) deprecated in TS 5.x — use `nodenext` or `bundler`.

### Project References (Monorepo)

```json
{
  "compilerOptions": { "composite": true, "incremental": true },
  "references": [{ "path": "../shared" }, { "path": "../core" }]
}
```

Use `tsc --build` for incremental compilation.

## Type System

### Utility Types

| Type | Purpose |
|------|---------|
| `Partial<T>` | Make all properties optional |
| `Required<T>` | Make all properties required |
| `Readonly<T>` | Make all properties readonly |
| `Record<K,V>` | Object type with key K and value V |
| `Pick<T,K>` / `Omit<T,K>` | Select/exclude specific properties |
| `Extract<T,U>` / `Exclude<T,U>` | Extract/exclude types from union |
| `ReturnType<T>` | Get function return type |
| `Awaited<T>` | Unwrap Promise type |

### Generics

```typescript
// Basic generic with constraints
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
  return obj[key];
}

// Multiple constraints
function merge<T extends object, U extends object>(a: T, b: U): T & U {
  return { ...a, ...b };
}
```

### Conditional Types

```typescript
type UnwrapPromise<T> = T extends Promise<infer U> ? U : T;
type ArrayElement<T> = T extends (infer E)[] ? E : never;
```

### Mapped Types

```typescript
// Key remapping
type Getters<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K];
};

// Filtering properties by type
type OnlyStrings<T> = {
  [K in keyof T as T[K] extends string ? K : never]: T[K];
};
```

### Type Guards

```typescript
// Custom type guard
function isCat(pet: Cat | Dog): pet is Cat {
  return (pet as Cat).meow !== undefined;
}

// typeof, instanceof, and in operator work as built-in guards
function process(value: string | number) {
  if (typeof value === "string") return value.toUpperCase();
  return value.toFixed(2);
}
```

### Branded Types

```typescript
type UserId = string & { readonly __brand: unique symbol };
type OrderId = string & { readonly __brand: unique symbol };

function createUserId(id: string): UserId { return id as UserId; }
```

Prevent mixing similar primitive types. Create branded types for domain primitives.

### Satisfies Operator

```typescript
const config = {
  endpoint: "/api",
  timeout: 3000,
} satisfies Record<string, string | number>;
// config.endpoint is inferred as "/api" (literal), not string
```

## Runtime Patterns

### Error Handling

```typescript
// Result type for functional error handling
type Result<T, E = Error> =
  | { success: true; data: T }
  | { success: false; error: E };

function parseJson<T>(json: string): Result<T> {
  try {
    return { success: true, data: JSON.parse(json) };
  } catch (e) {
    return { success: false, error: e as Error };
  }
}

// Error chaining with cause (ES2022+)
throw new Error("Failed to fetch data", { cause: originalError });
```

### Async Patterns

```typescript
// Parallel execution
const [users, posts] = await Promise.all([fetchUsers(), fetchPosts()]);

// Handle mixed success/failure
const results = await Promise.allSettled([fetchUser(1), fetchUser(2)]);
const successful = results
  .filter((r): r is PromiseFulfilledResult<User> => r.status === "fulfilled")
  .map((r) => r.value);

// Async generator for pagination
async function* paginate<T>(fetchPage: (page: number) => Promise<T[]>) {
  let page = 0;
  while (true) {
    const items = await fetchPage(page++);
    if (items.length === 0) break;
    yield* items;
  }
}
```

### Module Patterns

```typescript
// Named exports (preferred)
export const helper = () => {};
export type Config = { /* ... */ };

// Re-exports with type-only
export { util } from "./util.js";
export type { UtilOptions } from "./util.js";
```

Barrel files (`index.ts`) can hurt tree-shaking — use direct imports for large modules.

## Tooling

### ESLint (Flat Config)

```javascript
// eslint.config.js
import eslint from "@eslint/js";
import tseslint from "typescript-eslint";

export default tseslint.config(
  eslint.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  {
    languageOptions: {
      parserOptions: { projectService: true, tsconfigRootDir: import.meta.dirname },
    },
  }
);
```

Key rules: `no-explicit-any`, `no-floating-promises`, `strict-boolean-expressions`, `prefer-nullish-coalescing`.

### Build Tools

| Tool | Command | Purpose |
|------|---------|---------|
| tsc | `tsc` / `tsc --build` | Compile / incremental build |
| tsc | `tsc --noEmit` | Type check only |
| tsx | `tsx src/index.ts` | Run TypeScript directly (esbuild) |
| tsup | `tsup` | Bundle TypeScript libraries (CJS + ESM + DTS) |
| vitest | `vitest` | Modern, fast test runner |

### Vitest Configuration

```typescript
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    globals: true,
    environment: "node",
    coverage: { provider: "v8", reporter: ["text", "json", "html"] },
  },
});
```

## Best Practices

- Use `satisfies` to check types without widening
- Create branded types for domain primitives (UserId, OrderId)
- Use Result types for error handling over exceptions
- Keep type definitions close to usage
- Export types separately with `export type` for clarity
- Prefer `const` assertions for literal types
- Prefer interfaces for public APIs, types for unions/utilities

## Anti-Patterns to Avoid

- **`any` abuse** — use `unknown` and narrow with type guards
- **Excessive `as` casts** — use type guards or proper typing
- **Implicit any** — enable strict mode, add explicit types
- **Barrel overuse** — use direct imports for large modules (tree-shaking)
- **Enums** — use `const` objects with `as const` (enums have runtime overhead)
- **Namespaces** — use ES module imports/exports
- **Decorator abuse** — prefer composition and higher-order functions

## Error Escalation

- **Low:** Minor type inference issue — add explicit type annotation
- **Medium:** Type error in implementation — fix type, verify with tsc
- **High:** Breaking type change in public API — stop, present migration options
- **Critical:** Type safety bypass with any or type assertion — block operation, require proper typing
