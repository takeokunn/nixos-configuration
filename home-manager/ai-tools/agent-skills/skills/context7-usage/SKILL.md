---
name: context7-usage
description: Context7 MCP documentation retrieval patterns for up-to-date library and API references. Use this skill whenever current library docs, API signatures, version-specific behavior, or migration notes are needed.
version: 2.0.0
---

<purpose>
Provide consistent, efficient usage patterns for Context7 MCP so agents can retrieve current documentation and avoid stale API assumptions. Context7 covers thousands of libraries with live documentation.
</purpose>

<tools>
  <tool>mcp__plugin_claude-code-home-manager_context7__resolve-library-id - Resolve library names to Context7-compatible IDs before fetching docs</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs - Fetch focused documentation for specific topics, versions, or APIs</tool>
</tools>

<when_to_use>
  <case>User asks about a library's API, config options, or best practices</case>
  <case>Code references an external dependency and current behavior needs verification</case>
  <case>Migrating between library versions (check breaking changes)</case>
  <case>Debugging "why doesn't this API work" — verify against current docs before assuming a bug</case>
  <case>ANY time you'd otherwise rely on training-data memory for library syntax</case>
</when_to_use>

<workflow>
  <phase name="resolve">
    <step order="1">
      <action>Resolve the library to a Context7-compatible ID</action>
      <tool>mcp__plugin_claude-code-home-manager_context7__resolve-library-id</tool>
      <output>Library ID string (e.g., /microsoft/typescript, /nixos/nixpkgs)</output>
    </step>
  </phase>
  <phase name="fetch">
    <step order="2">
      <action>Fetch targeted documentation with a specific topic</action>
      <tool>mcp__plugin_claude-code-home-manager_context7__query-docs</tool>
      <output>Current documentation for the requested topic</output>
    </step>
  </phase>
</workflow>

<known_library_ids>
  <description>Pre-resolved IDs for frequently used libraries in this repository. Use these directly without calling resolve-library-id.</description>
  <library name="TypeScript" id="/microsoft/typescript" trust="9.9" />
  <library name="Nixpkgs / NixOS" id="/nixos/nixpkgs" trust="9.0" />
  <library name="Home Manager" id="/nix-community/home-manager" trust="8.5" />
  <library name="Go (website)" id="/golang/website" trust="8.3" />
  <library name="Go (tools)" id="/golang/tools" trust="8.3" />
  <library name="Rust (The Book)" id="/rust-lang/book" trust="9.0" />
  <library name="Rust (std)" id="/rust-lang/rust" trust="9.5" />
  <library name="Swift" id="/apple/swift" trust="8.5" />
  <library name="PHP" id="/php/php-src" trust="8.0" />
  <library name="GHC / Haskell" id="/ghc/ghc" trust="7.5" />
</known_library_ids>

<patterns>
  <pattern name="resolve_then_fetch">
    <description>Standard two-step: resolve library ID, then fetch docs with a focused topic.</description>
    <example>
      Step 1: resolve-library-id for "typescript"
      → returns "/microsoft/typescript"
      Step 2: query-docs context7CompatibleLibraryID="/microsoft/typescript" topic="tsconfig moduleResolution nodenext"
    </example>
  </pattern>

  <pattern name="version_sensitive_queries">
    <description>When behavior differs by version, specify the version in the topic or use a versioned library ID.</description>
    <example>
      query-docs id="/microsoft/typescript" topic="TypeScript 6.0 breaking changes module resolution"
      query-docs id="/nixos/nixpkgs" topic="buildGoModule Go 1.26"
    </example>
  </pattern>

  <pattern name="migration_check">
    <description>Before upgrading a dependency, check migration notes.</description>
    <example>
      query-docs id="/microsoft/typescript" topic="TypeScript 5 to 6 migration breaking changes"
      query-docs id="/rust-lang/rust" topic="Rust edition 2021 to 2024 migration"
    </example>
  </pattern>

  <pattern name="ecosystem_packaging">
    <description>For nixpkgs language packaging (buildGoModule, rustPlatform, etc.), always use Context7.</description>
    <example>
      query-docs id="/nixos/nixpkgs" topic="buildGoModule vendorHash cargoHash"
      query-docs id="/nixos/nixpkgs" topic="rustPlatform.buildRustPackage importCargoLock"
    </example>
  </pattern>
</patterns>

<decision_tree name="context7_usage">
  <question>Do you need current library/framework API behavior?</question>
  <branch condition="Yes, for a known library">Use known_library_ids list for direct query-docs call</branch>
  <branch condition="Yes, for an unknown library">Call resolve-library-id first, then query-docs</branch>
  <branch condition="No, need project-local evidence">Use Serena or Read/Bash tools instead</branch>
</decision_tree>

<best_practices>
  <practice priority="critical">Always use Context7 before making claims about external library APIs — training data may be stale</practice>
  <practice priority="critical">Use specific topic strings (not generic) for focused, relevant results</practice>
  <practice priority="high">Check migration docs when upgrading library versions</practice>
  <practice priority="high">For nixpkgs language packaging, Context7 is the authoritative source — use it before writing any derivation</practice>
  <practice priority="medium">Prefer known library IDs to avoid resolve step when possible</practice>
</best_practices>

<anti_patterns>
  <avoid name="relying_on_memory">
    <description>Assuming API behavior from training data without retrieval</description>
    <instead>Always query Context7 for any library syntax, config options, or behavior questions</instead>
  </avoid>
  <avoid name="generic_topic_strings">
    <description>Using overly broad topic strings ("typescript docs", "rust docs")</description>
    <instead>Be specific: "TypeScript 6.0 verbatimModuleSyntax", "Rust async fn in trait 1.75"</instead>
  </avoid>
  <avoid name="skipping_resolve">
    <description>Guessing library IDs without using resolve-library-id</description>
    <instead>Use known_library_ids for common libraries, resolve-library-id for unknown ones</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Keep guidance evidence-based and version-aware — retrieve before claiming</rule>
  <rule>Use Context7 for ANY external library API claim; never rely on training-data memory alone</rule>
</rules>

<rules priority="standard">
  <rule>Prefer project conventions over generic defaults from docs</rule>
  <rule>Specify the exact version when querying version-sensitive behavior</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Library ID not found — try alternative name or resolve-library-id</example>
    <example severity="medium">Context7 returns empty or irrelevant results — refine topic string</example>
    <example severity="high">Conflicting information between Context7 and local code — flag for investigation</example>
    <example severity="critical">Unsafe or misleading API guidance affecting security or correctness</example>
  </examples>
</error_escalation>

<constraints>
  <must>Use resolve-library-id before query-docs when library ID is not in known_library_ids</must>
  <must>Prefer official or primary documentation sources</must>
  <must>State uncertainty when docs are unavailable or ambiguous</must>
  <avoid>Relying on stale memory of API signatures without retrieval</avoid>
  <avoid>Generic topic strings that return unfocused results</avoid>
</constraints>

<related_skills>
  <skill name="fact-check">Use with external verification workflows for non-library claims</skill>
  <skill name="serena-usage">Use for project-local evidence gathering (complement, not substitute)</skill>
  <skill name="nix-ecosystem">For nixpkgs packaging queries — always pair with Context7</skill>
</related_skills>

<related_agents>
  <agent name="explore">Use for local codebase context before external doc lookup</agent>
  <agent name="general-purpose">Use for synthesis when multiple doc sources conflict</agent>
  <agent name="validator">Cross-validates claims retrieved from Context7 against project code</agent>
</related_agents>
