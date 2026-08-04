---
name: TypeScript Ecosystem
description: This skill should be used when the user asks to "write typescript", "typescript config", "tsconfig", "project references", "type definition", "generics", "utility types", or works with TypeScript language patterns, monorepo build graphs, or package boundaries. Covers rootDir and package-boundary errors, separating the typecheck graph from the build graph, per-project tsbuildinfo, case-variant sibling filenames on case-insensitive filesystems, and source-aliased siblings needing full public exports. Also covers modelling data in types — exhaustive Record-keyed registries instead of a Partial map with a fallback, discriminated unions with one uniform terminal payload field, ordinal position as a persisted encoding, and parsers where an absent field is valid but a malformed one is fatal.
version: 2.3.0
---

<purpose>
Provide comprehensive patterns for TypeScript language, configuration, type system, and tooling integration.
</purpose>

<tools>
  <tool>Read - Analyze tsconfig.json and TypeScript source files</tool>
  <tool>Edit - Modify TypeScript configurations and source code</tool>
  <tool>Bash - Run tsc, tsx, eslint, and build commands</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Fetch latest TypeScript documentation</tool>
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
      <version node="26" current="true" target="ES2025">Current release - use ES2025 for latest features</version>
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
    <version node="26" current="true">
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
      <note>Node.js 26 current release - use ES2025 target/lib for latest features</note>
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
      <warning>baseUrl: deprecated in TS 6.x and expected to be removed in TS 7.x; prefer paths without baseUrl</warning>
      <warning>moduleResolution: "node" (alias "node10"): removed in TS 6.0; use "nodenext" or "bundler"</warning>
      <decision_tree name="when_to_use">
        <question>Are you using a bundler or working with Node.js modules?</question>
        <if_yes>Configure appropriate moduleResolution: bundler for bundlers, nodenext for Node.js</if_yes>
        <if_no>Stick with default module resolution for simple projects</if_no>
      </decision_tree>
    </pattern>

    <pattern name="basenames_must_differ_by_more_than_case">
      <description>
        The default filesystems on macOS and Windows are case-INsensitive but
        case-preserving. Two modules whose basenames differ only in case are distinct files to
        the editor and to version control, but the same path to the resolver — so a helper
        placed beside a component with a case-variant name can resolve to the component and
        import itself.
      </description>
      <example>
        // Same directory, case-insensitive filesystem:
        //   NotificationBanner.tsx   (component)
        //   notificationBanner.ts    (helper)  ← ./notificationBanner may resolve to the .tsx
        //
        // Safe: give the helper a distinct name.
        //   NotificationBanner.tsx
        //   notification-banner-format.ts
      </example>
      <warning>
        The failure presents as a circular import or an undefined export, never as "file not
        found", so it reads like a bug in the module rather than in its name. It is also
        platform-dependent: it can pass on a case-sensitive CI runner and fail on developer
        machines, or the reverse. Watch the common `Foo.tsx` component / `foo.ts` helper pairing.
      </warning>
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

    <pattern name="separate_typecheck_and_build_graphs">
      <description>
        Typechecking and building want different file sets: the typecheck must include tests
        (otherwise test code is never checked), and the production build must exclude them
        (otherwise test types ship). Serving both from one config forces a choice between the
        two. Run two configs — a test-inclusive one for `--noEmit` checking and a
        production-only one for emit.
      </description>
      <example>
        // tsconfig.json — typecheck graph: sources + tests, no emit
        { "compilerOptions": { "noEmit": true, "incremental": true,
        "tsBuildInfoFile": "./.tsbuildinfo.check" },
        "include": ["src", "test"] }

        // tsconfig.build.json — build graph: sources only
        { "extends": "./tsconfig.json",
        "compilerOptions": { "noEmit": false, "outDir": "./dist",
        "tsBuildInfoFile": "./.tsbuildinfo.build" },
        "include": ["src"] }
      </example>
      <warning>
        Give each config its OWN tsBuildInfoFile. Two configs sharing one incremental state
        file silently clobber each other, so every run is a cold rebuild — there is no error,
        no warning, and the only symptom is that incremental builds are mysteriously slow. A
        single hardcoded `.tsbuildinfo` is exactly the configuration that causes this.
      </warning>
    </pattern>

    <pattern name="respect_the_package_boundary">
      <description>
        In a monorepo, TS6059 ("file is not under rootDir") and TS6307 ("file is not listed
        within the file list") mean the compilation has reached outside the package root. The
        instinctive fix — widening `rootDir` — dissolves the very boundary that produced the
        error and turns one package's build into a build of its neighbours. Fix the entrypoints
        instead.
      </description>
      <causes>
        <cause>Importing a sibling package's subpath implementation file (`pkg/src/internal/x`) instead of its declared public entrypoint.</cause>
        <cause>Importing another package's test tree (`pkg/test/helpers`) from this package's tests.</cause>
        <cause>A test-only path alias that resolves outside the package root, dragging those files into the program.</cause>
      </causes>
      <rule>Route every cross-package import through the sibling's public entrypoint and wire packages together with project `references`. If a test helper is needed in two packages, either copy the small helper locally or promote it to a public export — never reach into a neighbour's test tree.</rule>
    </pattern>
  </project_references>
</tsconfig>

<monorepo_version_skew>
  <principle>
    In a package graph, the code that runs is decided by where a value is CONSTRUCTED and by
    what the installer actually resolved — neither of which is visible in the consumer's
    dependency list. A graph that typechecks cleanly can still run two copies of the same
    library, or run an older one than the manifest suggests. These bugs are expensive because
    every artifact you would normally consult says the change was applied.
  </principle>

  <pattern name="runtime_version_follows_the_constructor">
    <description>
      A package that CONSTRUCTS a stateful service must itself depend on the exact API version
      the host expects. Adding a newer direct dependency to the host does not upgrade an object
      built inside a producer package that still resolves the older copy — the host holds a new
      type and an old implementation. The same shape appears with data tables: the host is
      pinned to the new table while the producer package that reads it resolves the previous
      one.
    </description>
    <rule>When upgrading a shared API, bump it in every package that constructs values of that API, not only in the package that consumes them. Verify against the installed tree (the package manager's `why`/`ls` output), not against the manifests.</rule>
    <note>
      The type-level variant is equally common: two installed copies of a nominally identical
      closed union are distinct types, so literals that share names produce incompatible public
      unions and an error message that reads as if the two are unrelated.
    </note>
  </pattern>

  <pattern name="source_aliased_siblings_need_full_public_exports">
    <description>
      Aliasing sibling packages to their SOURCE is the standard monorepo fast-feedback setup,
      and its standard failure is a green typecheck with a runtime explosion: the dev-time
      resolver reaches a symbol through source that the published entrypoint does not export.
      Every symbol the running application touches must be exported from the sibling's public
      entrypoint — a transitive dependency's internal export may typecheck in isolation and be
      unreachable once the real resolution rules apply.
    </description>
    <note>Alias the package that OWNS a service as well as its consumer. A half-aliased graph lets stale nested declarations resolve alongside the aliased source, producing structurally incompatible versions of the same service type at the composition boundary.</note>
  </pattern>

  <pattern name="vocabulary_changes_are_vertical_and_bottom_up">
    <description>
      Adding a member to a closed vocabulary (a shared union of string literals, an id space, a
      status set) is not a local edit. Add the canonical literal in the package that OWNS the
      vocabulary, release it, update any mirrored vocabularies and rules in the intermediate
      packages, release those, and only then pin and integrate at the host. Any other order
      produces a window where the type and the runtime disagree about what values exist.
    </description>
    <rule>Never mirror a closed union downstream — including in runtime guards. Import both the type and its guard from the owning package, so a newly registered member is immediately valid everywhere without a synchronized multi-package edit.</rule>
    <note>Adding a member to an exported closed union is an additive public API change (semver MINOR). While a package is on 0.x, a MINOR-classified change maps to a PATCH bump, which routinely surprises people reading the version alone.</note>
  </pattern>
</monorepo_version_skew>

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

  <exhaustive_registries>
    <principle>
      When one logical variant must be registered in several parallel places — a renderer
      table, a capability list, a serializer map — a checklist in a document decays and an
      exhaustive mapped type does not. Type each registry as `Record&lt;Union, T&gt;` so that
      adding a member to the union turns every unupdated registry into a compile error instead
      of a runtime hole.
    </principle>

    <pattern name="record_over_partial_map">
      <description>Prefer a total `Record&lt;Union, T&gt;` over a partial map or a lookup function with a fallback. The fallback is what converts a missing registration from a build failure into a silent default at runtime.</description>
      <example>
        type Shape = "circle" | "square" | "triangle";

        // Total: omitting a member is a compile error.
        const renderers: Record&lt;Shape, Renderer&gt; = {
        circle: renderCircle,
        square: renderSquare,
        triangle: renderTriangle,
        };

        // Partial: a missing member is `undefined` at runtime, and nothing complains.
        // const renderers: Partial&lt;Record&lt;Shape, Renderer&gt;&gt; = { circle: renderCircle };
      </example>
      <note>The same reasoning favors exhaustive `switch` with a `never`-typed default assertion over a `default` arm that quietly absorbs new members.</note>
    </pattern>

    <pattern name="ordinal_position_as_persisted_encoding">
      <description>
        When a union member's ORDINAL POSITION is the persisted encoding (an index into a
        codec table, a wire tag derived from array order), inserting a member is a data
        migration, not an edit — every previously stored value after the insertion point now
        decodes as its neighbour.
      </description>
      <rule>Append only. Any parallel array keyed by that ordinal must be extended at the same index in the same change. If a member must be removed, retire the slot rather than compacting the list.</rule>
      <note>List order also becomes observable wherever selection uses modulo or round-robin over the list, so even a pure append can perturb behavior and tests that depended on the cycle.</note>
    </pattern>
  </exhaustive_registries>

  <discriminated_unions>
    <principle>
      A discriminated union that is consumed by GENERIC machinery — a reducer, an event bus, a
      stream transition applicator — should carry its payload in a field named for the payload's
      STRUCTURAL ROLE, not for its domain meaning. Naming the terminal payload after what it
      happens to contain in one variant forces every generic consumer to branch on the
      discriminant it was written specifically to avoid branching on.
    </principle>

    <pattern name="uniform_terminal_payload_field">
      <description>Give every variant of a stream/result union the same terminal payload field, so one applicator can forward it for any variant.</description>
      <example>
        // Forces ad-hoc branching in every generic consumer:
        // | { kind: "done"; outputPath: string }
        // | { kind: "done"; summary: Summary }

        // Uniform: the applicator forwards `value` for either stream.
        type Transition&lt;T&gt; =
        | { kind: "chunk"; text: string }
        | { kind: "done"; value: T }
        | { kind: "error"; error: Error };
      </example>
      <note>Domain-meaningful names still belong INSIDE the payload type. The rule is about the field the generic layer reads, not about erasing meaning from the data.</note>
    </pattern>
  </discriminated_unions>

  <schema_single_source>
    <principle>
      Define a value's shape once, as a runtime schema, and derive everything else from it. A schema library (e.g. Zod, Valibot, ArkType) produces both a runtime validator and the static type via inference, so the type and the validation can never drift apart. Never hand-maintain a separate interface alongside a validator — that is two sources of truth for one shape.
    </principle>

    <pattern name="derive_type_from_schema">
      <description>Write the schema, then infer the type from it rather than declaring the type independently.</description>
      <example>
        const UserSchema = z.object({
        id: z.string().uuid(),
        email: z.string().email(),
        });
        type User = z.infer&lt;typeof UserSchema&gt;; // stays in lockstep with the validator
      </example>
    </pattern>

    <pattern name="machine_readable_field_descriptions">
      <description>Attach descriptions to schema fields so the same definition powers validation, static types, and generated artifacts (OpenAPI, JSON Schema, form metadata, LLM tool schemas). One annotated schema replaces separate docs.</description>
      <example>
        const CreateOrder = z.object({
        amount: z.number().int().describe("Total in minor currency units"),
        });
      </example>
    </pattern>

    <pattern name="normalize_validation_result">
      <description>When one endpoint accepts multiple input encodings, validate each branch against its schema and normalize all branches into a single validated result object of one shape, so downstream code never re-inspects which encoding arrived.</description>
    </pattern>

    <pattern name="absent_is_valid_malformed_is_fatal">
      <description>
        "Optional" is ambiguous between ABSENT and PRESENT-BUT-INVALID, and parsers routinely
        collapse the two by omitting whatever fails to parse. That lenient reading turns a
        producer's bug into silent data loss: the field disappears, the object still validates,
        and nothing downstream can tell that anything was dropped. Decide explicitly, and
        prefer the strict reading — absence is valid, malformed presence is an error.
      </description>
      <rule>An optional field that is present with the wrong type must fail the parse, not be dropped. In list processing this matters most: silently skipping malformed items yields a plausible-looking short result that no consumer can distinguish from a genuinely short one, so a malformed item should fail the whole read rather than shrink it.</rule>
      <note>Where partial results genuinely are acceptable, return the rejected items alongside the accepted ones so the caller can decide. The rule is against dropping them invisibly, not against tolerance itself.</note>
    </pattern>
  </schema_single_source>
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

    <pattern name="guarded_subscription">
      <description>
        Subscribing to an event source has three races that the obvious code loses. Install the
        release handle BEFORE subscribing, make release idempotent, and let only the currently
        active release clear the shared handle.
      </description>
      <example>
        // BAD: a source that emits synchronously during subscribe() fires before
        // `release` has been assigned, so the handler runs with no way to tear down.
        // const release = source.subscribe(handler);

        // GOOD: the handle exists before any event can arrive.
        const ref: { current: (() =&gt; void) | null } = { current: null };

        const start = () =&gt; {
        let unsubscribe: (() =&gt; void) | null = null;
        const release = () =&gt; {
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
      </example>
      <notes>
        <item>Race 1 — synchronous emission during `subscribe()`: the source fires before the assignment completes, so the handle must be installed first.</item>
        <item>Race 2 — cancellation during an awaited registration: teardown can run before registration resolves, so the late-registration branch must also release.</item>
        <item>Race 3 — superseded teardown: if any release may clear the shared handle, an old subscription's cleanup tears down its replacement. Guard on identity, as above. This is the subtle one and it is rarely written down.</item>
        <item>Stale events from a replaced subscription must be ignored by identity, not by a boolean flag that the replacement also sets.</item>
      </notes>
    </pattern>
  </resource_management>

  <browser_storage>
    <principle>
      In a transactional browser store, request success and durability are different events.
      A wrapper that resolves on the request's success callback reports "durable" when it only
      knows "accepted", and the write can still be rolled back afterwards.
    </principle>

    <pattern name="resolve_writes_on_transaction_complete">
      <description>
        For IndexedDB, a write must resolve only after the enclosing readwrite `IDBTransaction`
        fires `complete`. `IDBRequest.onsuccess` means the operation was accepted WITHIN the
        transaction; a later `abort` or `error` on that transaction must surface to the caller
        as a storage failure. Reads are the asymmetric case: a readonly request may resolve
        from `onsuccess`, because there is nothing to roll back.
      </description>
      <example>
        // Writes: wait for the transaction, not the request.
        const put = (store: IDBObjectStore, value: unknown) =&gt;
        new Promise&lt;void&gt;((resolve, reject) =&gt; {
        store.put(value);
        store.transaction.oncomplete = () =&gt; resolve();
        store.transaction.onabort = () =&gt; reject(store.transaction.error);
        store.transaction.onerror = () =&gt; reject(store.transaction.error);
        });
      </example>
      <warning>The asymmetry is what makes a naive uniform wrapper wrong: applying the request-level resolution to both reads and writes looks consistent and reports false durability, while applying transaction-level resolution to reads needlessly serializes them.</warning>
    </pattern>
  </browser_storage>

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

<server_client_boundary>
  <principle>
    In full-stack frameworks (React Router v7, Remix, Next.js) a single codebase compiles for two targets. Server-only code — DB clients, secrets, node: modules — must never reach the client bundle. Enforce this with an explicit boundary the bundler understands, not with the hope that tree-shaking drops unused code. Correctness must not depend on an optimization.
  </principle>

  <pattern name="server_file_naming">
    <description>Name server-only modules with a `.server.ts` suffix (or place them under a `.server/` directory). Framework bundlers treat these as a hard boundary and exclude them from the browser build. Server-only route exports (loader, action, headers) and imports of server-only packages are the other two recognized forms of server-only code.</description>
    <example>
      app/
      ├── auth.server.ts       // never shipped to the client
      ├── database.server.ts
      └── routes/
      └── dashboard/
      ├── loader.server.ts
      └── action.server.ts
    </example>
    <note>Do not rely on tree-shaking for correctness: a value that must stay server-side belongs behind the `.server` boundary, even if it currently looks unused on the client.</note>
  </pattern>

  <pattern name="forbid_server_barrel_value_imports">
    <description>A barrel (index.ts) that re-exports server implementations alongside client-safe types will pull the server code into the client bundle the moment any client-reachable file does a value import from it. Forbid value imports of server-mixed barrels from client-reachable code with a lint rule; allow type-only imports.</description>
    <example>
      // eslint.config.js — no-restricted-imports with allowTypeImports
      // Applies to client-reachable files (everything except *.server.ts):
      // - forbid value imports of barrels that re-export .server modules
      // - permit `import type { ... }` from the same barrel
      // Client code should import from safe subpaths (e.g. pkg/domain, pkg/application)
      // rather than the package root barrel.
    </example>
  </pattern>

  <pattern name="strict_content_type_handling">
    <description>On request handlers, accept an explicit allow-list of content types (application/json, multipart/form-data, application/x-www-form-urlencoded) and reject the rest, rather than auto-guessing. Parse and validate each accepted branch, then normalize to one validated result. Prefer form-data-only handling for in-page forms; add JSON support only where a non-browser/external caller genuinely needs it.</description>
  </pattern>
</server_client_boundary>

<edge_isolate_runtime>
  <principle>
    Edge/isolate runtimes (Cloudflare Workers and similar V8-isolate platforms) are not Node.js. They have no synchronous filesystem, `import.meta.url` is undefined, and process.env is not the source of configuration. Code that assumes the Node module/file model fails at module-initialization time — before any request is handled — which makes the failure look like a deploy error rather than a runtime one.
  </principle>

  <pattern name="no_runtime_file_or_path_resolution">
    <description>Do not read files or resolve paths at runtime in an isolate. Because `import.meta.url` is undefined there, any path derived from it (or any createRequire(import.meta.url) a bundler emits for CJS interop) throws at load. Inline content at build time via static imports so the bundler embeds it as a string constant.</description>
    <example>
      // Fails on isolates: resolves a path from import.meta.url at runtime.
      // const text = readFileSync(new URL("./prompt.md", import.meta.url));

      // Works: content is a static import, inlined at build time.
      import { promptTemplates } from "./prompts.js";
    </example>
    <note>If a dependency crashes at load with an undefined-path error, the usual cause is a bundler defaulting to a Node platform target; target a web/neutral platform so no code depends on a runtime `import.meta.url`.</note>
  </pattern>

  <pattern name="inject_config_per_request_not_at_module_init">
    <description>Isolates provide secrets/config through a per-request env binding, not process.env at module load. A module-level singleton constructed from process.env will read empty values. Export factory functions that accept config explicitly and construct per request.</description>
    <example>
      // Fails on isolates: reads the key at module-init from process.env.
      // export const client = makeClient(process.env.API_KEY);

      // Works: factory receives the key from the request-time env binding.
      export const createClient = (apiKey: string) =&gt; makeClient(apiKey);
      // handler: const client = createClient(env.API_KEY);
    </example>
  </pattern>
</edge_isolate_runtime>

<auth_security_patterns>
  <principle>
    Authentication boundaries fail open when a check is merely structural rather than cryptographic, or when security-relevant state lives somewhere that does not survive scale-out. Treat every externally supplied token and every cross-request nonce as adversarial.
  </principle>

  <pattern name="verify_external_idp_token_signatures">
    <description>An ID token from an external identity provider must be signature-verified — fetch the provider's JWKS, verify the RS256/ES256 signature, and handle key id (kid) rotation — before trusting any claim. Parsing the payload and checking its structure is not verification; it accepts forged tokens.</description>
  </pattern>

  <pattern name="oauth_state_in_shared_store">
    <description>Store OAuth `state` (and similar one-time nonces) in a shared store (Redis/DB) and delete it on first use. A process-local Map breaks under multiple instances or a restart, and a state that cannot be reliably validated cannot protect against CSRF on the callback.</description>
  </pattern>

  <pattern name="cookie_auth_hardening">
    <description>Cookie-authenticated mutating routes need CSRF/Origin verification (return 403 on failure). Set JWTs in HTTPOnly cookies from the server; the client only navigates/redirects. Synchronize the JWT `exp` and the cookie Max-Age from one source so they cannot drift. Store only a hash (e.g. SHA-256) of refresh tokens and rotate them one-time (invalidate on use, issue a new one).</description>
  </pattern>

  <pattern name="one_rotation_recovery_window">
    <description>
      One-time rotation (invalidate the presented token, issue a new one) has a liveness hole:
      if the response carrying the new token is lost — a dropped connection, a backgrounded
      tab — the client is locked out holding a token the server has already consumed, and no
      amount of retrying helps. Retain the hash of the IMMEDIATELY PREVIOUS token and accept it
      for exactly one recovery rotation, which closes the hole without accepting unbounded
      replay.
    </description>
    <rule>Store hashes only, keep exactly one generation of history (not a list), and reissue on a previous-generation hit rather than treating it as an attack. Anything beyond one generation is a replay window, not a recovery window.</rule>
    <notes>
      <item>The client half matters too: hold a registration/bootstrap secret in memory only until acceptance, and clear it from session storage at start-up, so a consumed secret cannot be replayed from disk.</item>
      <item>Be explicit about the residual edge this does not fix: if the very FIRST issuance is lost, the client never held a token to recover with, and only an administrative path can re-provision it. Naming that limit is part of the design, not a gap in it.</item>
    </notes>
  </pattern>

  <pattern name="decouple_optional_dependencies">
    <description>Do not let one login method's required environment (e.g. an embedded mini-app / in-app-webview integration) throw at dependency construction and take down an unrelated login path (e.g. email). Validate a subsystem's environment only when that subsystem is actually used.</description>
  </pattern>
</auth_security_patterns>

<hexagonal_ports>
  <principle>
    Abstract every external dependency behind a port (an interface), with an adapter per environment. This is what makes an application testable and portable across runtimes: the domain talks to ports, and only the composition root knows which adapter is wired in. Crucially, non-obvious externalities — the clock, timers, randomness — are dependencies too and should be ports.
  </principle>

  <pattern name="port_the_timer_and_clock">
    <description>Wrap timers/intervals in a TimerPort (e.g. setInterval that returns a cleanup function, plus lifecycle observation) rather than calling the platform timer directly. Periodic and time-dependent behavior then becomes deterministic in tests via fake timers.</description>
    <example>
      interface TimerPort {
      setInterval(fn: () =&gt; void, ms: number): () =&gt; void; // returns cleanup
      }
      // test: vi.useFakeTimers() drives the port deterministically
    </example>
  </pattern>

  <pattern name="inject_ports_via_factories">
    <description>Provide ports through factory functions / constructor injection rather than module-level singletons. This composes naturally with per-request injection on isolate runtimes and with mocking in tests, and keeps intermediate types (raw parsed rows, DTOs) from leaking platform concerns into the domain.</description>
  </pattern>
</hexagonal_ports>

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
        <description>TypeScript execution with esbuild (fallback for Node.js &lt; 24 or code using non-erasable syntax)</description>
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
    <step order="1">
  <action>Resolve library ID if needed (already known: /microsoft/typescript)</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>Fetch documentation with specific topic</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
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
  <practice priority="high">Use TS 6.x as the current baseline in this repository and prepare for TS 7.0 migration</practice>
  <practice priority="medium">Use Result types for error handling over exceptions</practice>
  <practice priority="medium">Keep type definitions close to usage</practice>
  <practice priority="medium">Export types separately with 'export type'</practice>
  <practice priority="medium">Use 'const' assertions for literal types</practice>
  <practice priority="medium">Prefer interfaces for public APIs, types for unions/utilities</practice>
  <practice priority="high">Give every tsconfig its own tsBuildInfoFile; a shared one silently disables incremental builds</practice>
  <practice priority="high">Fix rootDir/file-list errors by tightening cross-package entrypoints, never by widening rootDir</practice>
  <practice priority="high">Type cross-cutting registries as Record&lt;Union, T&gt; so a new variant fails the build rather than the runtime</practice>
  <practice priority="high">Treat an optional field that is present with the wrong type as a parse error, not as absent</practice>
  <practice priority="medium">Bump a shared API in every package that constructs its values, not only in the consumer</practice>
  <practice priority="medium">Resolve transactional-store writes on transaction completion, not on request success</practice>
  <practice priority="medium">Install a subscription's release handle before subscribing, and make release idempotent</practice>
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

  <avoid name="widening_rootdir_to_silence_boundary_errors">
    <description>Raising rootDir (or adding a neighbour's sources to include) to make TS6059/TS6307 go away</description>
    <instead>Import through the sibling package's public entrypoint and wire the packages with project references</instead>
  </avoid>

  <avoid name="shared_tsbuildinfo">
    <description>Two tsconfigs pointing at the same tsBuildInfoFile</description>
    <instead>One incremental state file per config; sharing one makes every build cold with no diagnostic</instead>
  </avoid>

  <avoid name="case_variant_sibling_filenames">
    <description>Two modules in one directory whose basenames differ only in case</description>
    <instead>Distinct basenames — on a case-insensitive filesystem the resolver treats them as one path and a module can import itself</instead>
  </avoid>

  <avoid name="partial_registry_with_fallback">
    <description>A partial map plus a default arm for a variant registry</description>
    <instead>A total Record&lt;Union, T&gt;, so an unregistered variant is a compile error rather than a silent default</instead>
  </avoid>

  <avoid name="mirrored_closed_union">
    <description>Re-declaring a shared closed union (or its runtime guard) in a downstream package</description>
    <instead>Import both from the owning package; a mirror guarantees a skew window on every addition</instead>
  </avoid>

  <avoid name="domain_named_terminal_payload">
    <description>Naming a discriminated union's terminal payload after its domain meaning when generic machinery consumes it</description>
    <instead>A uniform structural field across variants, with the domain names inside the payload type</instead>
  </avoid>

  <avoid name="dropping_malformed_optional_fields">
    <description>Parsers that omit an optional field when it is present but the wrong type</description>
    <instead>Fail the parse; silent omission converts a producer bug into invisible data loss</instead>
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
  <rule>Keep cross-package imports on public entrypoints; never reach into a sibling's internals or test tree</rule>
  <rule>Make registries exhaustive so cross-cutting additions fail at compile time</rule>
  <rule>Verify a dependency upgrade against the installed tree, not against the manifests</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Understand TypeScript code requirements</objective>
    <step order="1">
  <action>1. Check tsconfig.json for project settings</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Review existing type patterns in project</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Identify type dependencies and imports</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="implement">
    <objective>Write type-safe TypeScript code</objective>
    <step order="1">
  <action>1. Define types before implementation</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Use strict type checking features</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Follow project naming conventions</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="validate">
    <objective>Verify TypeScript correctness</objective>
    <step order="1">
  <action>1. Run tsc --noEmit for type checking</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Check with ESLint for style issues</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Verify tests pass</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor type inference issue</example>
    <example severity="medium">Type error in implementation</example>
    <example severity="high">Breaking type change in public API</example>
    <example severity="critical">Type safety bypass with any or type assertion</example>
  </examples>
</error_escalation>

<constraints>
  <must>Enable strict mode in tsconfig.json</must>
  <must>Define explicit types for public APIs</must>
  <must>Use type guards for runtime type checking</must>
  <avoid>Using any type without justification</avoid>
  <avoid>Type assertions without validation</avoid>
  <avoid>Ignoring TypeScript errors with ts-ignore</avoid>
  <avoid>Widening rootDir or include to silence a package-boundary error</avoid>
  <avoid>Restating framework-neutral state, durability, or untrusted-input rules that other skills own</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Symbol-level navigation for type definitions and interfaces</skill>
  <skill name="context7-usage">Fetch latest TypeScript compiler and tooling documentation</skill>
  <skill name="investigation-patterns">Debug type errors and investigate compilation issues</skill>
  <skill name="effect-ts">Effect (Effect-TS) service, Layer, Schema, and error-channel patterns built on top of TypeScript</skill>
  <skill name="state-transactions">Framework-neutral state ownership, atomicity, durability ordering, and schema-evolution rules — the browser-storage pattern here is the API mechanism, not the policy</skill>
  <skill name="trust-boundaries">Untrusted-input discipline that the schema and auth patterns here implement</skill>
  <skill name="quality-tools">Lint and refactoring gates that mechanically enforce the boundary rules above</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
