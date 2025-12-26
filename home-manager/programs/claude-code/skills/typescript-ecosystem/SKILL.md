---
name: TypeScript Ecosystem
description: This skill should be used when the user asks to "write typescript", "typescript config", "tsconfig", "type definition", "generics", "utility types", or works with TypeScript language patterns and configuration. Provides comprehensive TypeScript ecosystem patterns and best practices.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for TypeScript language, configuration, type system, and tooling integration.
</purpose>

<tsconfig>
<recommended_base>
<description>Node.js 22+ recommended configuration</description>
<config>
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
</config>
</recommended_base>

<strict_options>
<option name="strict">Enables all strict type-checking options</option>
<option name="strictNullChecks">null and undefined handled explicitly</option>
<option name="strictFunctionTypes">Stricter function type checking</option>
<option name="strictBindCallApply">Check bind, call, apply methods</option>
<option name="strictPropertyInitialization">Class properties must be initialized</option>
<option name="noImplicitAny">Error on implicit any types</option>
<option name="noImplicitReturns">Error on missing return statements</option>
<option name="noImplicitThis">Error on implicit this</option>
<option name="noUncheckedIndexedAccess">Add undefined to index signatures</option>
<option name="noUnusedLocals">Error on unused local variables</option>
<option name="noUnusedParameters">Error on unused parameters</option>
</strict_options>

<module_resolution>
<pattern name="nodenext">
<description>Modern Node.js ESM resolution (recommended)</description>
<config>
{
  "compilerOptions": {
    "module": "nodenext",
    "moduleResolution": "nodenext"
  }
}
</config>
<note>Requires "type": "module" in package.json</note>
</pattern>

<pattern name="bundler">
<description>For projects using bundlers (Vite, esbuild, webpack)</description>
<config>
{
  "compilerOptions": {
    "module": "esnext",
    "moduleResolution": "bundler"
  }
}
</config>
</pattern>

<pattern name="path_aliases">
<description>Import path aliases</description>
<config>
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@/*": ["src/*"],
      "@components/*": ["src/components/*"]
    }
  }
}
</config>
<warning>baseUrl is deprecated in TS 7.0; prefer paths without baseUrl</warning>
</pattern>
</module_resolution>

<project_references>
<description>Monorepo and incremental builds</description>
<config>
{
  "compilerOptions": {
    "composite": true,
    "incremental": true,
    "tsBuildInfoFile": ".tsbuildinfo"
  },
  "references": [
    { "path": "../shared" },
    { "path": "../core" }
  ]
}
</config>
<command>tsc --build for incremental compilation</command>
</project_references>
</tsconfig>

<type_patterns>
<utility_types>
<type name="Partial&lt;T&gt;">Make all properties optional</type>
<type name="Required&lt;T&gt;">Make all properties required</type>
<type name="Readonly&lt;T&gt;">Make all properties readonly</type>
<type name="Record&lt;K,V&gt;">Object type with key K and value V</type>
<type name="Pick&lt;T,K&gt;">Select specific properties from T</type>
<type name="Omit&lt;T,K&gt;">Exclude specific properties from T</type>
<type name="Exclude&lt;T,U&gt;">Exclude types from union</type>
<type name="Extract&lt;T,U&gt;">Extract types from union</type>
<type name="NonNullable&lt;T&gt;">Remove null and undefined</type>
<type name="ReturnType&lt;T&gt;">Get function return type</type>
<type name="Parameters&lt;T&gt;">Get function parameter types as tuple</type>
<type name="Awaited&lt;T&gt;">Unwrap Promise type</type>
</utility_types>

<generics>
<pattern name="basic">
<example>
function identity&lt;T&gt;(arg: T): T {
  return arg;
}
</example>
</pattern>

<pattern name="constraints">
<example>
function getProperty&lt;T, K extends keyof T&gt;(obj: T, key: K): T[K] {
  return obj[key];
}
</example>
</pattern>

<pattern name="default_type">
<example>
interface Container&lt;T = string&gt; {
  value: T;
}
</example>
</pattern>

<pattern name="multiple_constraints">
<example>
function merge&lt;T extends object, U extends object&gt;(a: T, b: U): T &amp; U {
  return { ...a, ...b };
}
</example>
</pattern>
</generics>

<conditional_types>
<pattern name="basic">
<syntax>T extends U ? X : Y</syntax>
<example>
type IsString&lt;T&gt; = T extends string ? true : false;
</example>
</pattern>

<pattern name="infer">
<description>Extract types within conditional</description>
<example>
type UnwrapPromise&lt;T&gt; = T extends Promise&lt;infer U&gt; ? U : T;
type ArrayElement&lt;T&gt; = T extends (infer E)[] ? E : never;
</example>
</pattern>

<pattern name="distributive">
<description>Distributes over union types</description>
<example>
type ToArray&lt;T&gt; = T extends any ? T[] : never;
// ToArray&lt;string | number&gt; = string[] | number[]
</example>
</pattern>
</conditional_types>

<mapped_types>
<pattern name="basic">
<example>
type Readonly&lt;T&gt; = {
  readonly [P in keyof T]: T[P];
};
</example>
</pattern>

<pattern name="key_remapping">
<example>
type Getters&lt;T&gt; = {
  [K in keyof T as `get${Capitalize&lt;string &amp; K&gt;}`]: () =&gt; T[K];
};
</example>
</pattern>

<pattern name="filtering">
<example>
type OnlyStrings&lt;T&gt; = {
  [K in keyof T as T[K] extends string ? K : never]: T[K];
};
</example>
</pattern>
</mapped_types>

<template_literal_types>
<pattern name="basic">
<example>
type EventName = `on${Capitalize&lt;string&gt;}`;
type Locale = `${Language}-${Country}`;
</example>
</pattern>

<pattern name="inference">
<example>
type ExtractRouteParams&lt;T&gt; = T extends `${string}:${infer Param}/${infer Rest}`
  ? Param | ExtractRouteParams&lt;Rest&gt;
  : T extends `${string}:${infer Param}`
  ? Param
  : never;
</example>
</pattern>
</template_literal_types>

<type_guards>
<pattern name="typeof">
<example>
function process(value: string | number) {
  if (typeof value === "string") {
    return value.toUpperCase();
  }
  return value.toFixed(2);
}
</example>
</pattern>

<pattern name="instanceof">
<example>
function handle(error: Error | string) {
  if (error instanceof Error) {
    return error.message;
  }
  return error;
}
</example>
</pattern>

<pattern name="custom">
<example>
interface Cat { meow(): void; }
interface Dog { bark(): void; }

function isCat(pet: Cat | Dog): pet is Cat {
  return (pet as Cat).meow !== undefined;
}
</example>
</pattern>

<pattern name="in_operator">
<example>
function move(animal: Fish | Bird) {
  if ("swim" in animal) {
    animal.swim();
  } else {
    animal.fly();
  }
}
</example>
</pattern>
</type_guards>

<branded_types>
<description>Nominal typing via branding</description>
<example>
type UserId = string &amp; { readonly __brand: unique symbol };
type OrderId = string &amp; { readonly __brand: unique symbol };

function createUserId(id: string): UserId {
  return id as UserId;
}
</example>
<use_case>Prevent mixing similar primitive types</use_case>
</branded_types>

<satisfies_operator>
<description>Type checking without widening</description>
<example>
const config = {
  endpoint: "/api",
  timeout: 3000,
} satisfies Record&lt;string, string | number&gt;;
// config.endpoint is inferred as "/api" (literal), not string
</example>
</satisfies_operator>
</type_patterns>

<runtime_patterns>
<error_handling>
<pattern name="result_type">
<description>Rust-inspired Result type for error handling</description>
<example>
type Result&lt;T, E = Error&gt; =
  | { success: true; data: T }
  | { success: false; error: E };

function parseJson&lt;T&gt;(json: string): Result&lt;T&gt; {
  try {
    return { success: true, data: JSON.parse(json) };
  } catch (e) {
    return { success: false, error: e as Error };
  }
}
</example>
</pattern>

<pattern name="custom_errors">
<example>
class ValidationError extends Error {
  constructor(
    message: string,
    public readonly field: string,
    public readonly code: string
  ) {
    super(message);
    this.name = "ValidationError";
  }
}
</example>
</pattern>

<pattern name="error_cause">
<description>Error chaining with cause (ES2022+)</description>
<example>
try {
  await fetchData();
} catch (e) {
  throw new Error("Failed to fetch data", { cause: e });
}
</example>
</pattern>
</error_handling>

<async_patterns>
<pattern name="promise_all">
<example>
const [users, posts] = await Promise.all([
  fetchUsers(),
  fetchPosts(),
]);
</example>
</pattern>

<pattern name="promise_allSettled">
<description>Handle mixed success/failure</description>
<example>
const results = await Promise.allSettled([
  fetchUser(1),
  fetchUser(2),
  fetchUser(3),
]);

const successful = results
  .filter((r): r is PromiseFulfilledResult&lt;User&gt; =&gt; r.status === "fulfilled")
  .map((r) =&gt; r.value);
</example>
</pattern>

<pattern name="async_iterator">
<example>
async function* paginate&lt;T&gt;(fetchPage: (page: number) =&gt; Promise&lt;T[]&gt;) {
  let page = 0;
  while (true) {
    const items = await fetchPage(page++);
    if (items.length === 0) break;
    yield* items;
  }
}

for await (const item of paginate(fetchUsers)) {
  console.log(item);
}
</example>
</pattern>
</async_patterns>

<module_patterns>
<pattern name="esm_exports">
<example>
// Named exports
export const helper = () =&gt; {};
export type Config = { /* ... */ };

// Default export
export default class Service {}

// Re-exports
export { util } from "./util.js";
export type { UtilOptions } from "./util.js";
</example>
</pattern>

<pattern name="barrel_exports">
<description>index.ts for clean imports</description>
<example>
// src/components/index.ts
export { Button } from "./Button.js";
export { Input } from "./Input.js";
export type { ButtonProps, InputProps } from "./types.js";
</example>
<warning>Can impact tree-shaking; use sparingly</warning>
</pattern>

<pattern name="dynamic_import">
<example>
const module = await import("./heavy-module.js");
module.doSomething();
</example>
</pattern>
</module_patterns>
</runtime_patterns>

<tooling>
<eslint>
<recommended_config>
<description>ESLint with TypeScript (flat config)</description>
<example>
// eslint.config.js
import eslint from "@eslint/js";
import tseslint from "typescript-eslint";

export default tseslint.config(
  eslint.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  }
);
</example>
</recommended_config>

<key_rules>
<rule name="@typescript-eslint/no-explicit-any">Prefer unknown over any</rule>
<rule name="@typescript-eslint/no-unused-vars">Detect unused variables</rule>
<rule name="@typescript-eslint/strict-boolean-expressions">Require explicit boolean conditions</rule>
<rule name="@typescript-eslint/no-floating-promises">Require awaiting promises</rule>
<rule name="@typescript-eslint/prefer-nullish-coalescing">Use ?? over ||</rule>
</key_rules>
</eslint>

<prettier>
<config>
{
  "semi": true,
  "singleQuote": false,
  "tabWidth": 2,
  "trailingComma": "es5",
  "printWidth": 100
}
</config>
</prettier>

<testing>
<vitest>
<description>Modern, fast test runner</description>
<config>
// vitest.config.ts
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    globals: true,
    environment: "node",
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
    },
  },
});
</config>
</vitest>

<jest>
<config>
// jest.config.ts
import type { Config } from "jest";

const config: Config = {
  preset: "ts-jest",
  testEnvironment: "node",
  roots: ["&lt;rootDir&gt;/src"],
  moduleNameMapper: {
    "^@/(.*)$": "&lt;rootDir&gt;/src/$1",
  },
};

export default config;
</config>
</jest>
</testing>

<build_tools>
<tsc>
<commands>
<command name="tsc">Compile TypeScript</command>
<command name="tsc --build">Incremental build (monorepo)</command>
<command name="tsc --noEmit">Type check only</command>
<command name="tsc --watch">Watch mode</command>
</commands>
</tsc>

<tsx>
<description>TypeScript execution with esbuild</description>
<commands>
<command name="tsx src/index.ts">Run TypeScript directly</command>
<command name="tsx watch src/index.ts">Watch mode</command>
</commands>
</tsx>

<tsup>
<description>Bundle TypeScript libraries</description>
<config>
// tsup.config.ts
import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/index.ts"],
  format: ["cjs", "esm"],
  dts: true,
  clean: true,
  sourcemap: true,
});
</config>
</tsup>
</build_tools>
</tooling>

<context7_integration>
<library_id>/microsoft/typescript</library_id>
<trust_score>9.9</trust_score>
<snippets>16397</snippets>

<usage_pattern>
<step>Resolve library ID if needed (already known: /microsoft/typescript)</step>
<step>Fetch documentation with specific topic</step>
<examples>
<example topic="tsconfig">Configuration options and patterns</example>
<example topic="generics">Generic type patterns</example>
<example topic="utility types">Built-in utility types</example>
<example topic="module resolution">Module resolution strategies</example>
</examples>
</usage_pattern>

<common_queries>
<query topic="strict mode">Strict compiler options</query>
<query topic="path mapping">Path aliases configuration</query>
<query topic="declaration files">Type declaration generation</query>
<query topic="nodenext">ESM support in Node.js</query>
</common_queries>
</context7_integration>

<anti_patterns>
<avoid name="any_abuse">
<description>Overusing 'any' defeats type safety</description>
<instead>Use 'unknown' and narrow with type guards</instead>
</avoid>

<avoid name="type_assertions">
<description>Excessive 'as' casts bypass type checking</description>
<instead>Use type guards or proper typing</instead>
</avoid>

<avoid name="implicit_any">
<description>Missing type annotations with noImplicitAny disabled</description>
<instead>Enable strict mode, add explicit types</instead>
</avoid>

<avoid name="barrel_overuse">
<description>Barrel files can hurt tree-shaking</description>
<instead>Use direct imports for large modules</instead>
</avoid>

<avoid name="enums">
<description>Enums have runtime overhead and quirks</description>
<instead>Use const objects with 'as const'</instead>
<example>
const Status = {
  Active: "active",
  Inactive: "inactive",
} as const;

type Status = (typeof Status)[keyof typeof Status];
</example>
</avoid>

<avoid name="namespace">
<description>Namespaces are legacy, use ES modules</description>
<instead>Use regular imports/exports</instead>
</avoid>

<avoid name="decorator_abuse">
<description>Decorators add complexity and are still experimental</description>
<instead>Prefer composition and higher-order functions</instead>
</avoid>
</anti_patterns>

<best_practices>
<practice>Enable strict mode in all projects</practice>
<practice>Use noUncheckedIndexedAccess for safer array/object access</practice>
<practice>Prefer 'unknown' over 'any' for unknown types</practice>
<practice>Use 'satisfies' to check types without widening</practice>
<practice>Create branded types for domain primitives</practice>
<practice>Use Result types for error handling over exceptions</practice>
<practice>Keep type definitions close to usage</practice>
<practice>Export types separately with 'export type'</practice>
<practice>Use 'const' assertions for literal types</practice>
<practice>Prefer interfaces for public APIs, types for unions/utilities</practice>
</best_practices>
