---
name: Serena Usage
description: This skill should be used when the user asks to "use serena", "semantic search", "symbol analysis", "find references", "code navigation", or needs Serena MCP guidance. Provides Serena tool usage patterns and orchestration integration.
version: 3.2.0
---

<purpose>
  Provide patterns for effective use of Serena MCP tools for semantic code operations, memory management, and orchestration workflow integration.
</purpose>

<tools>
  <tool name="activate_project">
    <description>Activate a Serena project for memory and symbol access</description>
    <param name="project">Project name to activate (required)</param>
    <use_case>First step in any session to enable Serena functionality</use_case>
  </tool>

  <tool name="check_onboarding_performed">
    <description>Verify if project onboarding has been completed</description>
    <use_case>After activate_project, ensure project is fully onboarded</use_case>
  </tool>

  <tool name="onboarding">
    <description>Perform initial project onboarding</description>
    <use_case>When check_onboarding_performed returns false</use_case>
  </tool>

  <tool name="get_symbols_overview">
    <description>Get high-level view of symbols in a file</description>
    <param name="relative_path">Path to file (required)</param>
    <param name="depth">Descendant depth, 0 for top-level only</param>
    <use_case>First step when exploring a new file; prefer depth=0 initially</use_case>
  </tool>

  <tool name="find_symbol">
    <description>Find symbols by name path pattern</description>
    <param name="name_path_pattern">Symbol name or path (e.g., "MyClass/myMethod")</param>
    <param name="relative_path">Restrict to file or directory</param>
    <param name="include_body">Include source code (default false)</param>
    <param name="depth">Include descendants</param>
    <param name="substring_matching">Match partial names (useful for uncertain names)</param>
    <use_case>Locate specific functions, classes, or methods</use_case>
  </tool>

  <tool name="find_referencing_symbols">
    <description>Find all references to a symbol</description>
    <param name="name_path">Symbol to find references for</param>
    <param name="relative_path">File containing the symbol</param>
    <use_case>Dependency analysis, impact assessment before refactoring</use_case>
  </tool>

  <tool name="replace_symbol_body">
    <description>Replace entire symbol definition</description>
    <param name="name_path">Symbol to replace</param>
    <param name="relative_path">File containing symbol</param>
    <param name="body">New symbol body</param>
    <use_case>Refactoring entire functions or classes</use_case>
  </tool>

  <tool name="insert_before_symbol">
    <description>Insert content before a symbol</description>
    <param name="name_path">Symbol to insert before</param>
    <param name="relative_path">File containing symbol</param>
    <param name="body">Content to insert</param>
    <use_case>Add imports, decorators, comments before a symbol</use_case>
  </tool>

  <tool name="insert_after_symbol">
    <description>Insert content after a symbol</description>
    <param name="name_path">Symbol to insert after</param>
    <param name="relative_path">File containing symbol</param>
    <param name="body">Content to insert</param>
    <use_case>Add new functions, classes after a symbol</use_case>
  </tool>

  <tool name="rename_symbol">
    <description>Rename symbol across codebase with automatic reference updates</description>
    <param name="name_path">Symbol to rename</param>
    <param name="relative_path">File containing symbol</param>
    <param name="new_name">New symbol name</param>
    <use_case>Consistent renaming with reference updates across all files</use_case>
  </tool>

  <tool name="list_memories">
    <description>List all available memory files</description>
    <use_case>Check existing patterns before implementation</use_case>
  </tool>

  <tool name="read_memory">
    <description>Read content of a memory file</description>
    <param name="memory_file_name">Name of memory file to read</param>
    <use_case>Load project patterns and conventions</use_case>
  </tool>

  <tool name="write_memory">
    <description>Write information to a memory file (creates or overwrites)</description>
    <param name="memory_file_name">Name of memory file to write</param>
    <param name="content">Content to write</param>
    <use_case>Record new patterns and conventions for future sessions</use_case>
  </tool>

  <tool name="edit_memory">
    <description>Replace content in a memory file using regex or literal matching</description>
    <param name="memory_file_name">Name of memory file to edit</param>
    <param name="needle">Pattern to search for</param>
    <param name="repl">Replacement string</param>
    <param name="mode">Either "literal" or "regex"</param>
    <use_case>Update specific parts of memory files without full rewrite</use_case>
  </tool>

  <tool name="rename_memory">
    <description>Rename a memory file</description>
    <param name="memory_file_name">Current name of the memory file</param>
    <param name="new_memory_file_name">New name for the memory file</param>
    <use_case>Rename memories for better organization or archival</use_case>
  </tool>

  <tool name="delete_memory">
    <description>Delete a memory file</description>
    <param name="memory_file_name">Name of memory file to delete</param>
    <use_case>Remove obsolete or incorrect patterns (requires user permission)</use_case>
  </tool>

  <tool name="get_current_config">
    <description>Get current Serena configuration for the active project</description>
    <use_case>Verify project settings and available language servers</use_case>
  </tool>

  <tool name="initial_instructions">
    <description>Read the Serena Instructions Manual</description>
    <use_case>Must be called at session start before any other Serena tool to load the manual</use_case>
  </tool>

  <tool name="find_declaration">
    <description>Find the declaration of a symbol using a regex with one capture group</description>
    <param name="relative_path">File containing the symbol reference (required)</param>
    <param name="regex">Regex with exactly one capture group matching the symbol; surround the group with enough context to make the match unambiguous (required)</param>
    <param name="containing_symbol_name_path">Optional: restrict search to body of a specific containing symbol</param>
    <param name="include_body">Include the declaration's source code (default false)</param>
    <param name="include_info">Include hover-like info (default false)</param>
    <use_case>Jump to declaration when you have a call site but not the definition location; example: to find the declaration of `process` in a call `obj.process(x)`, pass regex `"obj\.(process)\("`</use_case>
  </tool>

  <tool name="find_implementations">
    <description>Find all implementations of a symbol (e.g., interface or abstract class)</description>
    <param name="name_path">Symbol to find implementations for (required)</param>
    <param name="relative_path">File containing the symbol — must be a file, not a directory (required)</param>
    <param name="include_info">Include hover-like info about implementing symbols (default false)</param>
    <use_case>Discover all concrete implementations of an interface or abstract type</use_case>
  </tool>

  <tool name="get_diagnostics_for_file">
    <description>Get LSP diagnostics (errors, warnings, hints) for a file</description>
    <param name="relative_path">File to inspect (required)</param>
    <param name="min_severity">Minimum severity to include: 1=Error, 2=Warning, 3=Information, 4=Hint (default 4)</param>
    <param name="start_line">First 0-based line to include (default 0)</param>
    <param name="end_line">Last 0-based line to include, -1 means end of file (default -1)</param>
    <use_case>Verify a file is error-free after editing; use min_severity=2 to filter out hint noise</use_case>
  </tool>

  <tool name="replace_content">
    <description>Replace content in a file using regex or literal matching — the primary tool for sub-symbol edits</description>
    <param name="relative_path">File to edit (required)</param>
    <param name="needle">String or regex pattern to search for (required)</param>
    <param name="repl">Replacement string; regex mode supports backreferences as $!1, $!2, ... (required)</param>
    <param name="mode">Either "literal" or "regex" (required)</param>
    <param name="allow_multiple_occurrences">Replace all matches if true; error if multiple found when false (default false)</param>
    <use_case>Sub-symbol edits where replace_symbol_body would be too broad; prefer regex mode with wildcards (e.g., "beginning.*?end") to avoid specifying full content</use_case>
  </tool>

  <tool name="safe_delete_symbol">
    <description>Delete a symbol only if it has no references; returns reference list if unsafe</description>
    <param name="name_path_pattern">Symbol to delete (required)</param>
    <param name="relative_path">File containing the symbol (required)</param>
    <use_case>Safely remove dead code without risk of dangling references; inspect returned references before proceeding if deletion is blocked</use_case>
  </tool>
</tools>

<concepts>
  <concept name="memory_files">
    <description>Persistent storage for project patterns, conventions, and architectural decisions that survive across sessions</description>
    <example>
      list_memories  # Check existing patterns
      read_memory "nix-conventions"  # Load Nix patterns
      write_memory "api-patterns" "REST API conventions..."  # Record new pattern
      edit_memory "api-patterns" "old text" "new text" mode="literal"  # Update memory
      rename_memory "old-name" "new-name"  # Rename for organization
    </example>
  </concept>

  <concept name="memory_naming">
    <description>Consistent naming conventions for memory files. Names may include "/" to organize into subtopics (e.g., "codex/slash-command-mapping"); list_memories enumerates nested paths transparently.</description>
    <example>
      convention-{topic}     # Forward-looking project conventions (e.g., convention-nix-module-structure)
      decision-{topic}       # Architectural decision records (e.g., decision-use-home-manager)
      review-{topic}-YYYY-MM # Past investigation findings (e.g., review-nixvim-2026-05)
      {feature}-patterns     # Feature-specific reusable patterns
      {domain}-patterns      # Domain-specific patterns
      {project}-conventions  # (legacy) Project-wide conventions
      global/{topic}         # Shared across all projects, not just the active one — use only when the
                              # user explicitly instructs a memory to be project-independent (e.g., "global/nix/style-guide")
    </example>
  </concept>

  <concept name="symbol_path">
    <description>Path to symbol within a file using slash-separated hierarchy (e.g., MyClass/myMethod)</description>
    <example>
      find_symbol "MyClass/myMethod"  # Find specific method
      find_symbol "get*" substring_matching=true  # Find all getter methods
    </example>
  </concept>

  <concept name="replace_content_editing">
    <description>replace_content is the primary tool for sub-symbol edits — changes within a function body, a few lines inside a class, etc. Prefer regex mode with wildcards to avoid transcribing large sections of code.</description>
    <example>
      <note>Preferred: regex with wildcard to target a region</note>
      replace_content relative_path="src/main.ts" needle="function foo\(.*?\) \{.*?return old" repl="function foo() {\n  return new" mode="regex"

      <note>Literal match for exact short strings</note>
      replace_content relative_path="config.nix" needle="oldValue = true" repl="oldValue = false" mode="literal"
    </example>
  </concept>

  <concept name="language_support_architecture">
    <description>Serena resolves code intelligence through language servers (LSP). Each supported language is registered in a Language enum with a filename matcher (its file extensions) and mapped to a language-server class through a factory. A project has a set of active languages; symbol tools only work for files whose language is active and backed by a working LSP. This shapes both how the tool is extended and why it sometimes cannot help.</description>
    <adding_a_language>
      <note>Extending Serena to a new language is an LSP-integration task, not a parser-writing task. The decisive choices are which existing language server to wrap and how it is installed and launched.</note>
      <step>Wrap an existing language server rather than writing a parser. Prefer the single-core-dependency provider when the server is one executable or archive; use the multi-dependency base provider only when setup is genuinely complex.</step>
      <step>Register the language: add it to the Language enum with a filename matcher for its extensions, and add a factory branch that constructs the server.</step>
      <step>Provide a minimal test repository that exercises symbols, within-file references, and cross-file references.</step>
      <step>Write tests that assert the actual expected symbol names and reference locations were found. Asserting only that a non-empty list or a non-null result came back is insufficient and is the most common reason such contributions are rejected.</step>
    </adding_a_language>
    <selection_criteria>
      <criterion>Mandatory LSP capabilities for symbol tools are documentSymbol, definition, and references. A server missing any of these cannot back find_symbol or find_referencing_symbols and should be deferred, or offered only as experimental (completion/hover) support.</criterion>
      <criterion>Installation footprint (extra runtimes or package managers the server needs) determines adoption cost and CI feasibility.</criterion>
    </selection_criteria>
  </concept>
</concepts>

<patterns>
  <pattern name="session_initialization">
    <description>Initialize Serena at session start</description>
    <example>
      <step order="1">
        <action>Step 1: Read Serena manual</action>
      </step>
      initial_instructions

      <step order="2">
        <action>Step 2: Activate project</action>
      </step>
      activate_project project="project-name"

      <step order="3">
        <action>Step 3: Verify onboarding</action>
      </step>
      check_onboarding_performed

      <step order="4">
        <action>Step 4: If not onboarded, run onboarding</action>
      </step>
      onboarding (if needed)

      <step order="5">
        <action>Step 5: Check available memories</action>
      </step>
      list_memories
    </example>
  </pattern>

  <pattern name="explore_file">
    <description>Systematically explore file structure from high-level to detailed</description>
    <example>
      <step order="1">
        <action>Step 1: Get top-level overview</action>
      </step>
      get_symbols_overview relative_path="src/main.ts" depth=0

      <step order="2">
        <action>Step 2: Explore class members</action>
      </step>
      get_symbols_overview relative_path="src/main.ts" depth=1

      <step order="3">
        <action>Step 3: Get specific implementation</action>
      </step>
      find_symbol name_path_pattern="MyClass/myMethod" include_body=true
    </example>
  </pattern>

  <pattern name="trace_dependencies">
    <description>Trace symbol dependencies and callers</description>
    <example>
      <step order="1">
        <action>Step 1: Locate the symbol</action>
      </step>
      find_symbol name_path_pattern="processData" relative_path="src/processor.ts"

      <step order="2">
        <action>Step 2: Find all callers</action>
      </step>
      find_referencing_symbols name_path="processData" relative_path="src/processor.ts"

      <step order="3">
        <action>Step 3: Recursively trace for full dependency graph</action>
      </step>
      <note>Repeat steps 1-2 for each caller to build complete dependency tree</note>
    </example>
  </pattern>

  <pattern name="safe_refactoring">
    <description>Refactor with full impact analysis</description>
    <example>
      <step order="1">
        <action>Step 1: Understand current implementation</action>
      </step>
      find_symbol name_path_pattern="MyClass/oldMethod" include_body=true

      <step order="2">
        <action>Step 2: Identify all usages</action>
      </step>
      find_referencing_symbols name_path="MyClass/oldMethod" relative_path="src/myclass.ts"

      <step order="3">
        <action>Step 3: Perform the change</action>
      </step>
      replace_symbol_body name_path="MyClass/oldMethod" relative_path="src/myclass.ts" body="..."

      <step order="4">
        <action>Step 4: Update references if interface changed</action>
      </step>
      <note>If method signature changed, update all calling sites identified in step 2</note>
    </example>
  </pattern>

  <pattern name="file_based_editing">
    <description>Use replace_content for sub-symbol edits where replace_symbol_body would replace too much</description>
    <example>
      <step order="1">
        <action>Step 1: Locate the region to change with find_symbol (include_body=true)</action>
      </step>
      find_symbol name_path_pattern="MyClass/myMethod" include_body=true

      <step order="2">
        <action>Step 2: Construct a regex that uniquely targets the lines to replace</action>
      </step>
      <note>Use wildcards to avoid transcribing large sections; DOTALL and MULTILINE are enabled</note>

      <step order="3">
        <action>Step 3: Apply the replacement</action>
      </step>
      replace_content relative_path="src/main.ts" needle="oldPattern.*?boundary" repl="newContent" mode="regex"

      <step order="4">
        <action>Step 4: Verify with diagnostics</action>
      </step>
      get_diagnostics_for_file relative_path="src/main.ts" min_severity=2
    </example>
  </pattern>

  <pattern name="memory_workflow">
    <description>Use memories to maintain consistency across tasks</description>
    <example>
      <note>Before implementation</note>
      list_memories # Check what patterns exist
      read_memory "typescript-patterns" # Load relevant patterns

      <note>During implementation</note>
      <note>Follow the patterns loaded from memory</note>

      <note>After implementation</note>
      write_memory "api-client-pattern" "HTTP client pattern using fetch with retry logic..."
    </example>
  </pattern>

  <pattern name="memory_auto_creation_triggers">
    <description>When to automatically create or update memories</description>
    <should_create>
      <trigger>After discovering significant architectural pattern</trigger>
      <trigger>After resolving complex bug with reusable debugging insights</trigger>
      <trigger>After completing feature with reusable implementation pattern</trigger>
      <trigger>When user explicitly mentions a convention or preference</trigger>
      <trigger>After successful refactoring with transferable approach</trigger>
    </should_create>
    <should_not_create>
      <trigger>One-off fixes with no broader applicability</trigger>
      <trigger>User-specific temporary preferences</trigger>
      <trigger>Workarounds that should be replaced later</trigger>
      <trigger>Information already documented elsewhere</trigger>
    </should_not_create>
    <example>
      <note>Good: After discovering project uses specific error handling pattern</note>
      write_memory "error-handling-pattern" "# Error Handling Convention\n\nThis project uses Result type pattern with custom Error enum..."

      <note>Skip: After fixing typo in variable name</note>
      <note>No memory needed - not a reusable pattern</note>
    </example>
  </pattern>

  <pattern name="memory_reading_by_task_type">
    <description>Which memories to prioritize based on task type</description>
    <task_type name="investigation">
      <priority>1. {domain}-patterns (e.g., authentication-patterns)</priority>
      <priority>2. architecture-* (architectural decisions)</priority>
      <priority>3. {project}-conventions</priority>
    </task_type>
    <task_type name="implementation">
      <priority>1. {feature}-patterns (e.g., api-patterns)</priority>
      <priority>2. {language}-conventions (e.g., typescript-conventions)</priority>
      <priority>3. testing-patterns</priority>
    </task_type>
    <task_type name="review">
      <priority>1. {project}-conventions</priority>
      <priority>2. code-quality-* patterns</priority>
      <priority>3. architecture-* decisions</priority>
    </task_type>
    <task_type name="refactoring">
      <priority>1. architecture-* decisions</priority>
      <priority>2. {component}-patterns</priority>
      <priority>3. testing-patterns</priority>
    </task_type>
  </pattern>

  <pattern name="memory_content_format">
    <description>Standard YAML frontmatter for new Serena memory files, enabling structured filtering and lifecycle management</description>
    <frontmatter_template>
---
domain: &lt;nixvim | home-manager | ai-prompts | nix | general&gt;
status: active
created: YYYY-MM
last-verified: YYYY-MM
---
    </frontmatter_template>
    <fields>
      <field name="domain">Primary area this memory applies to (nixvim, home-manager, ai-prompts, nix, general)</field>
      <field name="status">active = current and verified; archived = superseded; draft = unverified hypothesis</field>
      <field name="created">Year-month the memory was first written (e.g., 2026-05)</field>
      <field name="last-verified">Year-month the content was last confirmed accurate; set equal to created on initial write</field>
    </fields>
    <rules>
      <rule>Apply to all new memories created via write_memory going forward</rule>
      <rule>Do NOT migrate existing memories retroactively — apply only to new entries</rule>
      <rule>When editing an existing memory that lacks frontmatter, add it at that time</rule>
      <rule>On write_memory: set last-verified = created</rule>
      <rule>On edit_memory: update last-verified to current YYYY-MM; leave created unchanged</rule>
    </rules>
  </pattern>

  <pattern name="memory_lifecycle">
    <description>Memory versioning, archival, and consolidation patterns. Freshness is maintained by two complementary mechanisms: memory_staleness_verification runs lazily during normal task execution against only the memories that task happened to read, while the /remember command performs a full periodic sweep across the entire memory index.</description>
    <versioning>
      <convention>Use date suffix for major updates: {name}-YYYY-MM</convention>
      <example>claude-code-architecture-2026-01</example>
      <note>For minor updates, use edit_memory instead of creating new version</note>
    </versioning>
    <archival>
      <trigger>When pattern is superseded by new approach</trigger>
      <action>Rename with -archived suffix using rename_memory OR delete if no historical value</action>
      <example>rename_memory "old-pattern" "old-pattern-archived"</example>
    </archival>
    <consolidation>
      <trigger>When multiple small memories cover related topics</trigger>
      <action>Merge into single comprehensive memory</action>
      <example>
        <note>Instead of: api-auth-pattern, api-error-pattern, api-retry-pattern</note>
        <note>Create: api-patterns with all three sections</note>
      </example>
    </consolidation>
  </pattern>

  <pattern name="memory_staleness_verification">
    <description>Lightweight, opportunistic freshness check applied only to memories actually read during a task — the lazy complement to the full sweep performed by the standalone /remember command. Piggybacks on read_memory calls already mandated by SERENA-B002, so it adds no extra memory reads of its own.</description>
    <trigger>A memory loaded via read_memory during this task was relied upon (its content informed a decision, an implementation choice, or an investigation finding)</trigger>
    <staleness_signal>
      <primary>Frontmatter last-verified field (see memory_content_format) — stale if more than 3 months old</primary>
      <fallback>If frontmatter is absent, treat the memory as a stale candidate on this basis alone and add frontmatter when editing (see memory_content_format rules)</fallback>
    </staleness_signal>
    <action_by_outcome>
      <outcome name="still_accurate">edit_memory to bump last-verified to the current YYYY-MM; add frontmatter first if it was missing</outcome>
      <outcome name="partially_outdated">edit_memory to correct the stale section and bump last-verified</outcome>
      <outcome name="fully_superseded">rename_memory with an "-archived" suffix; note the reason in output</outcome>
    </action_by_outcome>
    <scope_boundary>
      <in_scope>Only memories the task already read for its own purposes</in_scope>
      <out_of_scope>Proactively reading additional memories solely to check their freshness — that full-index sweep belongs to the /remember command, not to task execution</out_of_scope>
    </scope_boundary>
    <example>
      <note>During implementation, read_memory "nix-conventions" returns content with last-verified: 2026-02 (5 months old, current date 2026-07)</note>
      <note>Task confirms the described pattern still matches the codebase</note>
      edit_memory "nix-conventions" needle="last-verified: 2026-02" repl="last-verified: 2026-07" mode="literal"
    </example>
  </pattern>

  <pattern name="symbol_tools_unavailable_fallback">
    <description>In a multi-language repository, language detection may fix the project's active language on the dominant language. Symbol tools then fail for files of a secondary language — typically an error that reports the active languages and refuses to extract symbols for the target file. Treat this as an ongoing constraint of that repository, not a transient glitch to retry against.</description>
    <detection>
      <signal>get_symbols_overview or find_symbol errors with a message naming the active languages and an inability to extract symbols for the target file</signal>
      <signal>get_current_config shows the target file's language is not among the active languages</signal>
    </detection>
    <fallback>
      <step order="1">Locate definitions and references with Grep (rg) using symbol-name patterns, searching across both source and tests</step>
      <step order="2">Edit with text-based tools (replace_content in literal or regex mode, or the standard Edit tool) rather than replace_symbol_body or insert_after_symbol</step>
      <step order="3">Verify with the language's own build or load step, since get_diagnostics_for_file is also unavailable for the inactive language</step>
    </fallback>
    <note>Confirm the constraint once with get_current_config, then commit to the text-based path; do not repeatedly retry symbol tools that cannot work for an inactive language.</note>
  </pattern>

  <decision_tree name="serena_code_operation">
    <question>What type of code operation is needed?</question>
    <branch condition="Understand file structure">Use get_symbols_overview with depth=0, then depth=1</branch>
    <branch condition="Find specific symbol by name">Use find_symbol with name_path_pattern</branch>
    <branch condition="Read symbol implementation">Use find_symbol with include_body=true</branch>
    <branch condition="Find symbol references">Use find_referencing_symbols</branch>
    <branch condition="Replace entire symbol body">Use replace_symbol_body</branch>
    <branch condition="Add new code after symbol">Use insert_after_symbol</branch>
    <branch condition="Add new code before symbol">Use insert_before_symbol</branch>
    <branch condition="Rename symbol across codebase">Use rename_symbol</branch>
    <branch condition="Find declaration of a symbol from a call site">Use find_declaration with a regex capturing the symbol name</branch>
    <branch condition="Find implementations of an interface or abstract type">Use find_implementations</branch>
    <branch condition="Check file for errors after editing">Use get_diagnostics_for_file with min_severity=2</branch>
    <branch condition="Edit a few lines within a symbol body">Use replace_content with mode="regex" and wildcards</branch>
    <branch condition="Safely delete unused symbol">Use safe_delete_symbol; inspect returned references if blocked</branch>
    <branch condition="Find files or search patterns">Use Glob for file discovery, Grep for content search; follow up navigation with Serena tools</branch>
  </decision_tree>

  <decision_tree name="tool_selection">
    <question>What type of operation is needed?</question>
    <branch condition="Symbol lookup">Use find_symbol with name_path_pattern</branch>
    <branch condition="File structure overview">Use get_symbols_overview with appropriate depth</branch>
    <branch condition="Find references">Use find_referencing_symbols</branch>
    <branch condition="Refactor symbol">Use replace_symbol_body</branch>
    <branch condition="Rename symbol">Use rename_symbol for consistent updates</branch>
    <branch condition="Add code">Use insert_before_symbol or insert_after_symbol</branch>
    <branch condition="Check patterns">Use list_memories then read_memory</branch>
    <branch condition="Record patterns">Use write_memory</branch>
    <branch condition="Update patterns">Use edit_memory</branch>
    <branch condition="Find symbol declaration">Use find_declaration with regex</branch>
    <branch condition="Find implementations">Use find_implementations</branch>
    <branch condition="Check diagnostics">Use get_diagnostics_for_file</branch>
    <branch condition="Edit within symbol body">Use replace_content with regex mode</branch>
    <branch condition="Delete symbol safely">Use safe_delete_symbol</branch>
    <branch condition="Find files or search patterns">Use Glob (find) for files, Grep for content</branch>
  </decision_tree>

  <decision_tree name="serena_first_tool_selection">
    <description>Tool selection hierarchy: Serena for code intelligence, standard tools for filesystem navigation</description>
    <question>What type of operation is needed?</question>
    <branch condition="Find files by name pattern">
      <primary>Use Glob (find with name pattern)</primary>
      <reason>Serena no longer provides filesystem navigation tools; Glob is the correct tool for file discovery</reason>
    </branch>
    <branch condition="Search file contents for a text pattern">
      <primary>Use Grep for discovery</primary>
      <followup>Use Serena find_symbol or find_referencing_symbols for navigation after identifying location</followup>
    </branch>
    <branch condition="List directory contents">
      <primary>Use Bash ls</primary>
    </branch>
    <branch condition="View file structure/symbols">
      <primary>Use Serena get_symbols_overview (depth=0 first, then depth=1)</primary>
      <fallback condition="File is not code (YAML, JSON, MD) OR Serena unavailable">Use Grep for pattern discovery first, then Read for full context</fallback>
    </branch>
    <branch condition="Find specific function/class/method by name">
      <primary>Use Serena find_symbol with name_path_pattern</primary>
      <fallback condition="Symbol name unknown">Use find_symbol with substring_matching=true</fallback>
      <fallback condition="Still no results OR non-code file">Use Grep</fallback>
    </branch>
    <branch condition="Find declaration of a symbol from a call site">
      <primary>Use Serena find_declaration with regex capturing the symbol</primary>
    </branch>
    <branch condition="Find implementations of an interface or abstract type">
      <primary>Use Serena find_implementations</primary>
    </branch>
    <branch condition="Understand code in a file">
      <primary>Use Serena get_symbols_overview then find_symbol with include_body=true</primary>
      <fallback condition="Need full file context OR non-code file">Use Read</fallback>
    </branch>
    <branch condition="Find all usages of a symbol">
      <primary>Use Serena find_referencing_symbols</primary>
      <fallback condition="Symbol not in LSP scope">Use Grep with symbol name pattern</fallback>
    </branch>
    <branch condition="Edit entire symbol body">
      <primary>Use Serena replace_symbol_body</primary>
    </branch>
    <branch condition="Edit a few lines within a symbol">
      <primary>Use Serena replace_content with mode="regex" and wildcards</primary>
    </branch>
    <branch condition="Check file for errors after editing">
      <primary>Use Serena get_diagnostics_for_file with min_severity=2</primary>
    </branch>
    <branch condition="Delete a symbol that may have references">
      <primary>Use Serena safe_delete_symbol; inspect returned references if deletion is blocked</primary>
    </branch>
    <unavailable_conditions>
      <condition>Serena project not activated (call initial_instructions then activate_project first)</condition>
      <condition>File type not supported by LSP (use Grep or Read)</condition>
      <condition>Target file's language is not among the project's active languages — multi-language detection fixed on the dominant language; use Grep plus text edits (see symbol_tools_unavailable_fallback)</condition>
      <condition>Tool explicitly fails with error</condition>
    </unavailable_conditions>
  </decision_tree>

  <decision_tree name="language_specific_symbol_operations">
    <description>Language-specific guidance for symbol operations</description>
    <branch condition="Strongly typed languages (TypeScript, Go, Rust, Java)">
      <preference>Strongly prefer symbol operations (find_symbol, get_symbols_overview)</preference>
      <reason>LSP provides accurate symbol resolution</reason>
    </branch>
    <branch condition="Dynamic languages (Python, JavaScript, Ruby)">
      <preference>Use symbol operations with substring_matching=true</preference>
      <reason>Dynamic nature may require broader matching</reason>
    </branch>
    <branch condition="Configuration languages (Nix, YAML, JSON, TOML)">
      <preference>Use Grep for pattern discovery, then Read for full context when structure is complex</preference>
    </branch>
    <branch condition="Markup/Documentation (Markdown, RST, HTML)">
      <preference>Use Grep or Read</preference>
      <reason>Symbol operations less useful for prose</reason>
    </branch>
  </decision_tree>
</patterns>

<enforcement>
  <mandatory_behaviors>
    <behavior id="SERENA-B001" priority="critical">
      <trigger>Session start</trigger>
      <action>Call initial_instructions to read the Serena manual, then activate project with activate_project and check_onboarding_performed</action>
      <verification>Project activation recorded in output</verification>
    </behavior>
    <behavior id="SERENA-B002" priority="critical">
      <trigger>Before any implementation</trigger>
      <action>Check Serena memories with list_memories</action>
      <verification>Memory check recorded in output</verification>
    </behavior>
    <behavior id="SERENA-B005" priority="critical">
      <trigger>After significant pattern discovery or successful implementation</trigger>
      <action>Create or update memory with write_memory or edit_memory</action>
      <verification>Memory operation recorded in output for reusable patterns</verification>
      <guidance>See memory_auto_creation_triggers pattern for when to create</guidance>
    </behavior>
    <behavior id="SERENA-B007" priority="critical">
      <trigger>When examining code structure or navigating codebase</trigger>
      <action>Use get_symbols_overview or find_symbol before Read for code files</action>
      <verification>Symbol operations attempted before full file read</verification>
      <exception>Non-code files (YAML, JSON, MD) may use Read directly</exception>
    </behavior>
    <behavior id="SERENA-B008" priority="critical">
      <trigger>When saving findings to an existing memory topic</trigger>
      <action>First call list_memories to check if a memory for this topic exists;
        if it exists, use edit_memory to append or update sections;
        only use write_memory for genuinely new memory topics</action>
      <verification>edit_memory used for existing topics; write_memory only for new ones</verification>
    </behavior>
    <behavior id="SERENA-B009" priority="high">
      <trigger>After relying on the content of a memory that was read via read_memory during this task</trigger>
      <action>Follow memory_staleness_verification: check last-verified against a 3-month threshold; bump, correct, or archive as appropriate</action>
      <verification>Staleness check outcome noted in output (verified / updated / archived / already fresh)</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="SERENA-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Reading entire files when symbol operations suffice</action>
      <response>Use get_symbols_overview and find_symbol instead</response>
    </behavior>
    <behavior id="SERENA-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Deleting memories without explicit user request</action>
      <response>Use edit_memory instead or ask user permission</response>
    </behavior>
    <behavior id="SERENA-P004" priority="critical">
      <trigger>When reading code files</trigger>
      <action>Using Read for code file discovery instead of Serena symbol tools</action>
      <response>Use get_symbols_overview and find_symbol for code exploration; Read is forbidden for discovery of code files</response>
      <note>File/directory navigation (ls, find, grep) uses standard tools; Serena owns code intelligence</note>
    </behavior>
    <behavior id="SERENA-P006" priority="critical">
      <trigger>When exploring code files</trigger>
      <action>Using Read without first attempting get_symbols_overview or find_symbol</action>
      <response>Use symbol operations first; fall back to Read only if insufficient</response>
      <exception>Non-code files (YAML, JSON, MD, config) may use Read directly</exception>
    </behavior>
    <behavior id="SERENA-P007" priority="critical">
      <trigger>After discovering reusable patterns</trigger>
      <action>Failing to document significant patterns in memory</action>
      <response>Use write_memory to record reusable patterns for future sessions</response>
      <guidance>See memory_auto_creation_triggers pattern for guidance</guidance>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<best_practices>
  <practice priority="critical">Always activate project and check onboarding at session start (SERENA-B001)</practice>
  <practice priority="critical">Always check memories with list_memories and read_memory before implementing new features (SERENA-B002)</practice>
  <practice priority="critical">Use symbol operations (get_symbols_overview, find_symbol) over reading entire files (SERENA-B007)</practice>
  <practice priority="critical">Use Serena symbol tools for code intelligence; use Glob/Grep/ls for filesystem navigation (SERENA-P004)</practice>
  <practice priority="high">Restrict searches by relative_path when scope is known to improve performance</practice>
  <practice priority="high">Use substring_matching for uncertain symbol names to broaden search results</practice>
  <practice priority="high">Record significant patterns with write_memory after discovery (SERENA-B005)</practice>
  <practice priority="high">Verify freshness of memories read this task via memory_staleness_verification; bump last-verified, correct, or archive as needed (SERENA-B009)</practice>
  <practice priority="high">Use edit_memory for updating existing memories instead of delete and recreate</practice>
  <practice priority="high">Follow serena_first_tool_selection decision tree for consistent tool choices</practice>
  <practice priority="medium">Verify symbol changes with find_referencing_symbols before refactoring</practice>
  <practice priority="medium">Follow memory_reading_by_task_type to prioritize relevant memories</practice>
  <practice priority="medium">When a multi-language repo's language detection excludes the target file's language, fall back to Grep plus text edits instead of retrying symbol tools (symbol_tools_unavailable_fallback)</practice>
</best_practices>

<anti_patterns>
  <avoid name="reading_entire_files">
    <description>Reading entire files when only specific symbols are needed</description>
    <instead>Use get_symbols_overview for file structure and find_symbol with include_body for specific implementations</instead>
  </avoid>

  <avoid name="unscoped_searches">
    <description>Searching entire codebase when scope is known</description>
    <instead>Use relative_path parameter to restrict search to known files or directories</instead>
  </avoid>

  <avoid name="ignoring_memories">
    <description>Implementing features without checking existing patterns</description>
    <instead>Always check list_memories and read_memory before implementation</instead>
  </avoid>

  <avoid name="manual_refactoring">
    <description>Manually updating symbol references across files</description>
    <instead>Use rename_symbol for consistent renaming with automatic reference updates</instead>
  </avoid>

  <avoid name="excessive_depth">
    <description>Using high depth values unnecessarily in get_symbols_overview</description>
    <instead>Start with depth=0, then incrementally increase if needed</instead>
  </avoid>

  <avoid name="deleting_memories_without_permission">
    <description>Using delete_memory without explicit user request</description>
    <instead>Use edit_memory to update memories; only delete when user explicitly requests</instead>
  </avoid>

</anti_patterns>

<rules priority="critical">
  <rule>Always check memories before implementing new features (SERENA-B002)</rule>
  <rule>Use symbol operations (get_symbols_overview, find_symbol) over reading entire files (SERENA-B007, SERENA-P001)</rule>
  <rule>Use Serena symbol tools for code intelligence; use Glob/Grep/ls for filesystem navigation (SERENA-P004)</rule>
  <rule>Use Serena symbol editing (replace_symbol_body, replace_content, insert_after_symbol, insert_before_symbol, rename_symbol) for precise code modifications</rule>
  <rule>Record significant patterns with write_memory after discovery (SERENA-B005, SERENA-P007)</rule>
</rules>

<rules priority="standard">
  <rule>Restrict searches by relative_path when scope is known</rule>
  <rule>Use substring_matching for uncertain symbol names</rule>
  <rule>Use edit_memory for updating existing memories; delete_memory only when explicitly requested by user</rule>
  <rule>Follow serena_first_tool_selection decision tree for tool choices</rule>
  <rule>Follow language_specific_symbol_operations for language-appropriate tools</rule>
  <rule>Follow memory_reading_by_task_type for prioritizing which memories to read</rule>
  <rule>Follow memory_staleness_verification for memories read during a task; leave full-index sweeps to /remember</rule>
</rules>

<workflow>
  <phase name="prepare">
    <objective>Prepare for effective Serena tool usage</objective>
    <step order="1">
      <action>1. Read Serena manual with initial_instructions</action>
    </step>
    <step order="2">
      <action>2. Activate project with activate_project</action>
    </step>
    <step order="3">
      <action>3. Verify onboarding with check_onboarding_performed</action>
    </step>
    <step order="4">
      <action>4. Check list_memories for existing patterns</action>
    </step>
    <step order="5">
      <action>5. Read relevant memories with read_memory</action>
    </step>
    <step order="6">
      <action>6. Identify target symbols or files</action>
    </step>
    <step order="7">
      <action>7. Choose appropriate tool based on decision_tree</action>
    </step>
  </phase>
  <phase name="execute">
    <objective>Execute Serena operations efficiently</objective>
    <step order="1">
      <action>1. Start with get_symbols_overview for file structure</action>
    </step>
    <step order="2">
      <action>2. Use find_symbol with include_body for details</action>
    </step>
    <step order="3">
      <action>3. Use find_referencing_symbols for dependencies</action>
    </step>
    <step order="4">
      <action>4. Use symbol editing tools for modifications</action>
    </step>
  </phase>
  <phase name="verify">
    <objective>Verify results and record patterns</objective>
    <step order="1">
      <action>1. Record new patterns with write_memory</action>
    </step>
    <step order="2">
      <action>2. Verify file correctness with get_diagnostics_for_file after edits</action>
    </step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Symbol not found with exact match</example>
    <example severity="medium">Memory file not found</example>
    <example severity="high">Conflicting information in memories</example>
    <example severity="critical">Memory corruption or invalid state</example>
  </examples>
</error_escalation>

<constraints>
  <must>Follow serena_first_tool_selection decision tree for tool selection</must>
  <must>Activate project and check onboarding at session start (SERENA-B001)</must>
  <must>Check memories before implementing new features (SERENA-B002)</must>
  <must>Use symbol operations over reading entire files for code (SERENA-B007)</must>
  <must>Use Serena symbol tools for code intelligence; use Glob/Grep/ls for filesystem navigation (SERENA-P004)</must>
  <must>Record significant patterns with write_memory (SERENA-B005)</must>
  <must>Restrict searches by relative_path when scope is known</must>
  <avoid>Reading entire files when symbol operations suffice (SERENA-P001)</avoid>
  <avoid>Unscoped searches across entire codebase</avoid>
  <avoid>Ignoring existing memory patterns (SERENA-B002 violation)</avoid>
  <avoid>Deleting memories without explicit user request (SERENA-P003)</avoid>
  <avoid>Failing to document reusable patterns in memory (SERENA-P007)</avoid>
</constraints>

<related_skills>
  <skill name="investigation-patterns">Investigation methodology using Serena tools</skill>
  <skill name="core-patterns">Shared patterns for error escalation, decision criteria, enforcement</skill>
  <skill name="nix-ecosystem">Nix patterns stored in Serena memories</skill>
  <skill name="typescript-ecosystem">TypeScript patterns stored in Serena memories</skill>
  <skill name="golang-ecosystem">Go patterns stored in Serena memories</skill>
  <skill name="rust-ecosystem">Rust patterns stored in Serena memories</skill>
  <skill name="common-lisp-ecosystem">Common Lisp patterns stored in Serena memories</skill>
  <skill name="emacs-ecosystem">Emacs patterns stored in Serena memories</skill>
</related_skills>

<related_agents>
  <agent name="explore">Complement Serena symbol search with file-level exploration</agent>
  <agent name="general-purpose">Apply Serena-retrieved context to implementation tasks</agent>
</related_agents>
