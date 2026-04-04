---
name: TypeScript Ecosystem
description: This skill should be used when the user asks to "write typescript", "typescript config", "tsconfig", "type definition", "generics", "utility types", or works with TypeScript language patterns and configuration. Provides comprehensive TypeScript ecosystem patterns and best practices.
version: 2.0.0
---

<purpose>
Provide comprehensive patterns for TypeScript language, configuration, type system, and tooling integration.
</purpose>

<tools>
  <tool>Read - Analyze tsconfig.json and TypeScript source files</tool>
  <tool>Edit - Modify TypeScript configurations and source code</tool>
  <tool>Bash - Run tsc, tsx, eslint, and build commands</tool>
  <tool>mcp__context7__get-library-docs - Fetch latest TypeScript documentation</tool>
</tools>

<concepts>
  <concept name="strict_mode">Enable all strict checking options (strict: true) for maximum type safety</concept>
  <concept name="module_resolution">nodenext for ESM Node.js, bundler for Vite/esbuild/webpack projects</concept>
  <concept name="utility_types">Built-in generic types: Partial, Required, Pick, Omit, Record, Extract, Exclude, ReturnType</concept>
  <concept name="type_narrowing">Use type guards (typeof, instanceof, in, custom predicates) to safely narrow union types</concept>
</concepts>

<tsconfig>
  <recommended_base>
    <description>Node.js version-specific recommended configurations</description>
    <mapping>
      <version node="24" lts="true" target="ES2024">Active LTS - use ES2024 for stable features</version>
      <version node="26" upcoming="true" target="ES2025">Upcoming (April 2026) - use ES2025 for latest features</version>
    </mapping>
    <version node="24" lts="true">
      <config>
        {
        "compilerOptions": {
        "target": "ES2024",
        "lib": ["ES2024"],
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
      </config>
      <note>Node.js 24 Active LTS - use ES2024 target/lib for stable features</note>
    </version>
    <version node="26" upcoming="true">
      <config>
        {
        "compilerOptions": {
        "target": "ES2025",
        "lib": ["ES2025"],
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
      </config>
      <note>Node.js 26 (upcoming April 2026) - use ES2025 target/lib for latest features</note>
    </version>
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
      <example>
        {
        "compilerOptions": {
        "module": "nodenext",
        "moduleResolution": "nodenext"
        }
        }
      </example>
      <note>Requires "type": "module" in package.json</note>
    </pattern>

    <pattern name="bundler">
      <description>For projects using bundlers (Vite, esbuild, webpack)</description>
      <example>
        {
        "compilerOptions": {
        "module": "esnext",
        "moduleResolution": "bundler"
        }
        }
      </example>
    </pattern>

    <pattern name="path_aliases">
      <description>Import path aliases</description>
      <example>
        {
        "compilerOptions": {
        "baseUrl": ".",
        "paths": {
        "@/*": ["src/*"],
        "@components/*": ["src/components/*"]
        }
        }
        }
      </example>
      <warning>baseUrl: deprecated in TS 6.0 (latest stable), will be removed in TS 7.0; prefer paths without baseUrl</warning>
      <warning>moduleResolution: "node" (alias "node10"): removed in TS 6.0; use "nodenext" or "bundler"</warning>
      <decision_tree name="when_to_use">
        <question>Are you using a bundler or working with Node.js modules?</question>
        <if_yes>Configure appropriate moduleResolution: bundler for bundlers, nodenext for Node.js</if_yes>
        <if_no>Stick with default module resolution for simple projects</if_no>
      </decision_tree>
    </pattern>
  </module_resolution>

  <project_references>
    <description>Monorepo and incremental builds</description>
    <example>
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
    </example>
    <note>Use tsc --build for incremental compilation</note>
  </project_references>
</tsconfig>

<native_typescript>
  <description>Node.js 24+ runs TypeScript natively without flags or external runners. Node.js 22.6+ required --experimental-strip-types.</description>
  <pattern name="native_execution">
    <description>Run .ts files directly with Node.js 24+ (no tsx/ts-node needed)</description>
    <example>
      node src/index.ts
    </example>
    <note>Node.js strips type annotations at runtime. Code must be "erasable syntax only" — no enums, namespaces, or parameter properties with runtime semantics.</note>
  </pattern>
  <pattern name="erasable_syntax_only">
    <description>Enable erasableSyntaxOnly in tsconfig to ensure compatibility with Node.js native execution</description>
    <example>
      {
      "compilerOptions": {
      "erasableSyntaxOnly": true
      }
      }
    </example>
    <note>Errors on constructs that Node.js cannot strip (const enums, namespaces with runtime code, legacy parameter properties).</note>
  </pattern>
</native_typescript>

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
      <description>Basic generic function</description>
      <example>
        function identity&lt;T&gt;(arg: T): T {
        return arg;
        }
      </example>
    </pattern>

    <pattern name="constraints">
      <description>Generic with type constraints</description>
      <example>
        function getProperty&lt;T, K extends keyof T&gt;(obj: T, key: K): T[K] {
        return obj[key];
        }
      </example>
    </pattern>

    <pattern name="default_type">
      <description>Generic with default type parameter</description>
      <example>
        interface Container&lt;T = string&gt; {
        value: T;
        }
      </example>
    </pattern>

    <pattern name="multiple_constraints">
      <description>Multiple generic parameters with constraints</description>
      <example>
        function merge&lt;T extends object, U extends object&gt;(a: T, b: U): T &amp; U {
        return { ...a, ...b };
        }
      </example>
    </pattern>
  </generics>

  <conditional_types>
    <pattern name="basic">
      <description>Basic conditional type</description>
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
      <description>Basic mapped type</description>
      <example>
        type Readonly&lt;T&gt; = {
        readonly [P in keyof T]: T[P];
        };
      </example>
    </pattern>

    <pattern name="key_remapping">
      <description>Map keys with renaming</description>
      <example>
        type Getters&lt;T&gt; = {
        [K in keyof T as `get${Capitalize&lt;string &amp; K&gt;}`]: () =&gt; T[K];
        };
      </example>
    </pattern>

    <pattern name="filtering">
      <description>Filter properties by type</description>
      <example>
        type OnlyStrings&lt;T&gt; = {
        [K in keyof T as T[K] extends string ? K : never]: T[K];
        };
      </example>
    </pattern>
  </mapped_types>

  <template_literal_types>
    <pattern name="basic">
      <description>Template literal type construction</description>
      <example>
        type EventName = `on${Capitalize&lt;string&gt;}`;
        type Locale = `${Language}-${Country}`;
      </example>
    </pattern>

    <pattern name="inference">
      <description>Extract parameters from template literals</description>
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
      <description>Built-in typeof type guard</description>
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
      <description>Built-in instanceof type guard</description>
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
      <description>Custom type guard function</description>
      <example>
        interface Cat { meow(): void; }
        interface Dog { bark(): void; }

        function isCat(pet: Cat | Dog): pet is Cat {
        return (pet as Cat).meow !== undefined;
        }
      </example>
      <decision_tree name="when_to_use">
        <question>Does TypeScript need help narrowing union types?</question>
        <if_yes>Implement custom type guard with is predicate</if_yes>
        <if_no>Use built-in typeof or instanceof guards</if_no>
      </decision_tree>
    </pattern>

    <pattern name="in_operator">
      <description>Property existence type guard</description>
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
    <pattern name="branded_primitives">
      <description>Nominal typing via branding</description>
      <example>
        type UserId = string &amp; { readonly **brand: unique symbol };
        type OrderId = string &amp; { readonly **brand: unique symbol };

        function createUserId(id: string): UserId {
        return id as UserId;
        }
      </example>
      <note>Prevent mixing similar primitive types</note>
    </pattern>
  </branded_types>

  <satisfies_operator>
    <pattern name="type_checking_without_widening">
      <description>Type checking without widening</description>
      <example>
        const config = {
        endpoint: "/api",
        timeout: 3000,
        } satisfies Record&lt;string, string | number&gt;;
        // config.endpoint is inferred as "/api" (literal), not string
      </example>
    </pattern>
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
      <decision_tree name="when_to_use">
        <question>Do you need explicit error handling without exceptions?</question>
        <if_yes>Use Result type for functional error handling</if_yes>
        <if_no>Use try-catch for traditional exception handling</if_no>
      </decision_tree>
    </pattern>

    <pattern name="custom_errors">
      <description>Custom error classes with additional context</description>
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

  <resource_management>
    <pattern name="using_declaration">
      <description>Explicit Resource Management with using/await using (TC39 Stage 3, TS 5.2+)</description>
      <example>
        function readFile(path: string) {
        using file = openFile(path);
        return file.read();
        // file[Symbol.dispose]() called automatically
        }

        async function withConnection() {
        await using conn = await getConnection();
        return conn.query("SELECT 1");
        // conn[Symbol.asyncDispose]() called automatically
        }
      </example>
      <note>Requires "lib": ["ESNext.Disposable"] or target ES2024+. Use DisposableStack for managing multiple resources.</note>
    </pattern>
  </resource_management>

  <async_patterns>
    <pattern name="promise_all">
      <description>Parallel promise execution</description>
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
      <description>Async generator for pagination</description>
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
      <description>ES module export patterns</description>
      <example>
        // Named exports
        export const helper = () =&gt; {};
        export type Config = { /\* ... \_/ };

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
      <description>Code splitting with dynamic imports</description>
      <example>
        const module = await import("./heavy-module.js");
        module.doSomething();
      </example>
    </pattern>
  </module_patterns>
</runtime_patterns>

<tooling>
  <eslint>
    <pattern name="recommended_config">
      <description>ESLint 10 with TypeScript (flat config only; eslintrc fully removed in v10)</description>
      <example>
        // eslint.config.js
        import eslint from "@eslint/js";
        import tseslint from "typescript-eslint";
        import { defineConfig, globalIgnores } from "eslint/config";

        export default defineConfig([
        globalIgnores(["dist/", "node_modules/"]),
        eslint.configs.recommended,
        ...tseslint.configs.strictTypeChecked,
        {
        languageOptions: {
        parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
        },
        },
        },
        ]);
      </example>
      <note>ESLint 10 locates eslint.config.* from each linted file's directory, not cwd. defineConfig() and globalIgnores() provide type-safe config composition.</note>
    </pattern>

    <key_rules>
      <rule name="@typescript-eslint/no-explicit-any">Prefer unknown over any</rule>
      <rule name="@typescript-eslint/no-unused-vars">Detect unused variables</rule>
      <rule name="@typescript-eslint/strict-boolean-expressions">Require explicit boolean conditions</rule>
      <rule name="@typescript-eslint/no-floating-promises">Require awaiting promises</rule>
      <rule name="@typescript-eslint/prefer-nullish-coalescing">Use ?? over ||</rule>
    </key_rules>
  </eslint>

  <prettier>
    <pattern name="recommended_config">
      <description>Prettier configuration for TypeScript</description>
      <example>
        {
        "semi": true,
        "singleQuote": false,
        "tabWidth": 2,
        "trailingComma": "es5",
        "printWidth": 100
        }
      </example>
    </pattern>
  </prettier>

  <biome>
    <pattern name="recommended_config">
      <description>Biome — unified fast linter and formatter (alternative to ESLint+Prettier)</description>
      <example>
        // biome.json
        {
        "$schema": "https://biomejs.dev/schemas/2.0/schema.json",
        "linter": {
        "enabled": true,
        "rules": {
        "recommended": true
        }
        },
        "formatter": {
        "enabled": true,
        "indentStyle": "space",
        "indentWidth": 2
        }
        }
      </example>
      <note>Biome is significantly faster than ESLint+Prettier. Supports JS, TS, JSX, TSX, JSON, CSS. Use when unified tooling and speed are priorities.</note>
    </pattern>
  </biome>

  <testing>
    <vitest>
      <pattern name="config">
        <description>Vitest 4.x - fast test runner with stable Browser Mode (requires Vite >= 6.0, Node.js >= 20)</description>
        <example>
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
        </example>
        <note>Vitest 4 removed the experimental tag from Browser Mode. Visual regression testing available via @vitest/browser-playwright.</note>
      </pattern>
      <pattern name="workspace">
        <description>Vitest workspace for monorepo testing</description>
        <example>
          // vitest.workspace.ts
          import { defineWorkspace } from "vitest/config";

          export default defineWorkspace([
          "packages/*/vitest.config.ts",
          ]);
        </example>
      </pattern>
    </vitest>

    <jest>
      <pattern name="config">
        <description>Jest configuration for TypeScript</description>
        <example>
          // jest.config.ts
          import type { Config } from "jest";

          const config: Config = {
          preset: "ts-jest",
          testEnvironment: "node",
          roots: ["&lt;rootDir&gt;/src"],
          moduleNameMapper: {
          "^@/(.\*)$": "&lt;rootDir&gt;/src/$1",
          },
          };

          export default config;
        </example>
      </pattern>
    </jest>
  </testing>

  <build_tools>
    <tsc>
      <tool name="tsc">
        <description>TypeScript compiler commands</description>
        <use_case name="compile">tsc - Compile TypeScript</use_case>
        <use_case name="build">tsc --build - Incremental build (monorepo)</use_case>
        <use_case name="check">tsc --noEmit - Type check only</use_case>
        <use_case name="watch">tsc --watch - Watch mode</use_case>
      </tool>
    </tsc>

    <tsx>
      <tool name="tsx">
        <description>TypeScript execution with esbuild (fallback for Node.js < 24 or code using non-erasable syntax)</description>
        <use_case name="run">tsx src/index.ts - Run TypeScript directly</use_case>
        <use_case name="watch">tsx watch src/index.ts - Watch mode</use_case>
      </tool>
    </tsx>

    <tsup>
      <pattern name="config">
        <description>Bundle TypeScript libraries</description>
        <example>
          // tsup.config.ts
          import { defineConfig } from "tsup";

          export default defineConfig({
          entry: ["src/index.ts"],
          format: ["cjs", "esm"],
          dts: true,
          clean: true,
          sourcemap: true,
          });
        </example>
      </pattern>
    </tsup>
  </build_tools>
</tooling>

<typescript_versions>
  <current_stable>
    <version>TypeScript 6.0</version>
    <description>Final JavaScript-based compiler release. Bridge release between TS 5.x and 7.0. No 6.1 planned.</description>
    <install>npm install -D typescript</install>
  </current_stable>
  <upcoming>
    <version>TypeScript 7.0</version>
    <description>Go-native compiler rewrite with up to 10x performance improvement. New language service with ~8x project load time reduction.</description>
    <install>npm install -D @typescript/native-preview</install>
    <note>Near completion; nightly preview available. TS 7.0 is the future of TypeScript.</note>
  </upcoming>
</typescript_versions>

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
      <example topic="typescript 7">Go-native compiler migration</example>
    </examples>
  </usage_pattern>

  <common_queries>
    <query topic="strict mode">Strict compiler options</query>
    <query topic="path mapping">Path aliases configuration</query>
    <query topic="declaration files">Type declaration generation</query>
    <query topic="nodenext">ESM support in Node.js</query>
  </common_queries>
</context7_integration>

<best_practices>
  <practice priority="critical">Enable strict mode in all projects</practice>
  <practice priority="critical">Use noUncheckedIndexedAccess for safer array/object access</practice>
  <practice priority="high">Prefer 'unknown' over 'any' for unknown types</practice>
  <practice priority="high">Use 'satisfies' to check types without widening</practice>
  <practice priority="high">Create branded types for domain primitives</practice>
  <practice priority="high">Use TS 6.0 as current stable; prepare for TS 7.0 Go-native migration</practice>
  <practice priority="medium">Use Result types for error handling over exceptions</practice>
  <practice priority="medium">Keep type definitions close to usage</practice>
  <practice priority="medium">Export types separately with 'export type'</practice>
  <practice priority="medium">Use 'const' assertions for literal types</practice>
  <practice priority="medium">Prefer interfaces for public APIs, types for unions/utilities</practice>
</best_practices>

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
    <description>Decorators add complexity; use judiciously even though TC39 decorators are now stable (since TS 5.0)</description>
    <instead>Prefer composition and higher-order functions for simpler cases</instead>
  </avoid>

  <avoid name="eslintrc_config">
    <description>eslintrc configuration format is fully removed in ESLint 10</description>
    <instead>Use flat config with eslint.config.js and defineConfig()</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Enable strict mode in all TypeScript projects</rule>
  <rule>Never use any without documented justification; prefer unknown</rule>
  <rule>Run tsc --noEmit before committing to catch type errors</rule>
</rules>

<rules priority="standard">
  <rule>Use noUncheckedIndexedAccess for safer array/object access</rule>
  <rule>Export types separately with 'export type' for clarity</rule>
  <rule>Define types before implementation for better design</rule>
  <rule>Use satisfies operator to check types without widening</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand TypeScript code requirements</objective>
    <step>1. Check tsconfig.json for project settings</step>
    <step>2. Review existing type patterns in project</step>
    <step>3. Identify type dependencies and imports</step>
  </phase>
  <phase name="implement">
    <objective>Write type-safe TypeScript code</objective>
    <step>1. Define types before implementation</step>
    <step>2. Use strict type checking features</step>
    <step>3. Follow project naming conventions</step>
  </phase>
  <phase name="validate">
    <objective>Verify TypeScript correctness</objective>
    <step>1. Run tsc --noEmit for type checking</step>
    <step>2. Check with ESLint for style issues</step>
    <step>3. Verify tests pass</step>
  </phase>
</workflow>

<error_escalation>
  <level severity="low">
    <example>Minor type inference issue</example>
    <action>Add explicit type annotation</action>
  </level>
  <level severity="medium">
    <example>Type error in implementation</example>
    <action>Fix type, verify with tsc</action>
  </level>
  <level severity="high">
    <example>Breaking type change in public API</example>
    <action>Stop, present migration options to user</action>
  </level>
  <level severity="critical">
    <example>Type safety bypass with any or type assertion</example>
    <action>Block operation, require proper typing</action>
  </level>
</error_escalation>

<constraints>
  <must>Enable strict mode in tsconfig.json</must>
  <must>Define explicit types for public APIs</must>
  <must>Use type guards for runtime type checking</must>
  <avoid>Using any type without justification</avoid>
  <avoid>Type assertions without validation</avoid>
  <avoid>Ignoring TypeScript errors with ts-ignore</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Symbol-level navigation for type definitions and interfaces</skill>
  <skill name="context7-usage">Fetch latest TypeScript compiler and tooling documentation</skill>
  <skill name="investigation-patterns">Debug type errors and investigate compilation issues</skill>
</related_skills>
