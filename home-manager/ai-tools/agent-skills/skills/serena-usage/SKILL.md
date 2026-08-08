---
name: Serena Usage
description: This skill should be used when the user asks to "use serena", "semantic search", "symbol analysis", "find references", "code navigation", "serena memory", or needs Serena MCP guidance. Also covers organising a growing memory corpus as a reference graph — one root entry-point memory linking outward, references that describe what the target covers, and no memory stating when to read itself — instead of a flat set an agent must read entirely, and recovering a parallel subagent's report from its session transcript when the completion notification never arrives. Provides Serena tool usage patterns and orchestration integration. Keywords — a memory body stating current state rather than a changelog, storing the command instead of the count it produced, checking whether what a memory names still exists, stacked frontmatter blocks from a partial edit, finding a duplicate by symptom wording, and a shared active-project pointer misrouting a parallel session's lookup.
version: 3.7.0
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
      <trigger>A note that names one file and would not change what you do in a different file. That is a commit message. The trigger list above is monotone — every extraction is "a refactoring approach", every fix is "a bug insight" — so it can only ever argue for writing, and a corpus grown from it alone becomes an index no agent can afford to read, which pushes the next session toward reading nothing at all.</trigger>
      <trigger>Anything volatile enough to be wrong within weeks — line numbers, file counts, current status, an in-flight branch's state. Volatility is the load-bearing exclusion because it rejects at write time exactly the entries the staleness check would otherwise have to catch later.</trigger>
      <trigger>Generic language or framework knowledge, and facts a single quick read would establish. A memory earns its place by preventing an expensive rediscovery, not by recording something true.</trigger>
    </should_not_create>
    <register>
      <rule>Write dense agent notes, not prose documentation — invariants and terse bullets, with rationale and worked examples omitted unless they prevent a likely mistake. A maintained corpus looks like a handful of short files; an accumulated one looks like dozens of long ones.</rule>
    </register>
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

  <pattern name="memory_reference_graph">
    <description>Structure a memory corpus as a reference graph with a single root, not a flat set. At small scale a flat set is fine, because listing it is cheap. Past a few dozen memories, an agent that must read everything to find what matters pays that cost on every task, so the corpus needs to be traversable instead.</description>
    <structure>
      <element name="root">One designated entry-point memory that every session reads first. It holds no domain detail of its own; it links outward to the domain memories.</element>
      <element name="domain_memories">Linked from the root, each covering one area and linking on to more specific memories within it.</element>
      <element name="leaf_memories">Specific patterns, traps, and decisions, reached by traversal rather than by scanning the whole index.</element>
    </structure>
    <rules>
      <rule>A reference must carry a description of what the target covers, precise enough to decide whether to follow it. The target's name alone is not enough — a name tells you the topic, not whether the content bears on the question in hand.</rule>
      <rule>A memory should not contain instructions about when to read itself. That guidance belongs to the referrer, which is the only place with the context to judge relevance. Self-describing read conditions duplicate across every referrer and go stale independently of each other.</rule>
      <rule>When adding a memory, add the reference from its parent in the same edit. An unreferenced memory is unreachable by traversal and effectively invisible, however good its content.</rule>
      <rule>When writing a link, confirm the target exists, or say in the same line that it is a placeholder for a memory not yet written. A dangling reference reads exactly like a valid one until someone follows it, so the graph degrades silently rather than loudly.</rule>
      <rule>When a task finds itself reading more than a handful of memories on one topic, that is the signal to write the linking entry that gathers them — not to add another leaf. A cluster held together only by a shared filename prefix is not a graph.</rule>
    </rules>
    <reconciling_note>This does not replace memory_reading_by_task_type. The task-type priority lists are the selection heuristic for a flat corpus or when no root exists; the reference graph is how a corpus is navigated once one does. Where both apply, start at the root and let the task type decide which branches to follow.</reconciling_note>
  </pattern>

  <pattern name="parallel_subagent_result_recovery">
    <description>Recovering a parallel subagent's report when its completion notification does not arrive. Placed here because a lost report usually means the knowledge it carried never reaches a memory — the corpus loses the finding, not just the message.</description>
    <problem>Completion notifications for parallel subagents can be delayed or lost. The absence of a notification is therefore not evidence that the agent failed, and re-running on that assumption discards completed work and doubles the cost.</problem>
    <recovery>
      <step order="1">Look in the session directory's `subagents/` folder for the agent's `agent-*.jsonl` transcript. The final report survives there even when the notification did not arrive.</step>
      <step order="2">Check the transcript's modification time and tail. A recent mtime and a terminal assistant message mean the agent finished; a stalled mtime mid-run means it did not.</step>
      <step order="3">Extract the report as the longest assistant text message in the transcript. Agent reports are substantially longer than the intermediate status notes around them.</step>
    </recovery>
    <note>The sibling `.meta.json` records only spawn-time configuration — agent type, name, model, permission mode — and no completion state, so it cannot answer whether the agent finished. Read the transcript, not the metadata.</note>
    <rule>Check the transcript before concluding an agent failed and re-running it.</rule>
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
    <body_rules>
      <description>The frontmatter above constrains metadata only. These rules constrain the body, which is what a reader actually loads.</description>
      <rule name="body_states_current_state">A memory body is a document describing the present state, not a change log. If an addition invalidates something already written, rewriting or deleting that passage is part of the same edit. Appending looks like the safe move — it destroys no prior observation — but memories are read top-down under a context budget, so an append-only file becomes a document whose truth value decreases with reading order, and the part most likely to be read is the part most likely to be wrong.</rule>
      <rule name="retraction_goes_in_the_lead">When a task's observations contradict a memory, rewrite that memory's opening line to say so before doing anything else with it. A retraction buried under sixteen dated update markers does not reach a reader who stops after the first paragraph. This is also what makes a status field expendable — if the lead always states current status, nothing depends on a taxonomy that has to be maintained separately. Treat a status value that has never taken anything but its default across the whole corpus as decoration to route around, not a filter to trust.</rule>
      <rule name="store_the_command_not_the_count">Never write a figure that moves with the tree — test counts, file counts, lines of code, coverage percentage, dependency totals. It is stale one commit later, and a date stamp cannot protect it: last-verified is honest about when someone looked and useless for deciding whether to trust the number today. Store the selector and the runner instead. "The full unit suite is &lt;command&gt;, and it is expected to report zero unexpected results" survives every commit that a number does not.</rule>
      <rule name="record_the_re_verification_command">Record the exact command that establishes the memory's claim, verbatim, next to the claim. A date tells a later reader when someone was satisfied and offers them only two options, trust or re-derive; the command makes re-verification a paste. Where the memory records an audit or a survey, add the commit or date the audit covered and the command that reveals what has changed since, so a re-audit becomes a diff instead of a re-derivation.</rule>
      <rule name="name_what_decays_fastest">State explicitly which parts of the memory decay first — line numbers and counts always do — so a reader knows which sentences to re-check and which to rely on. A memory that does not distinguish its durable claims from its perishable ones gets discarded whole once any part of it is found wrong.</rule>
      <rule name="record_the_verification_set">When a memory covers an area of work, record what constituted done for it — the commands that had to pass, and any non-zero output that was accepted as normal. That accepted-warning detail is written nowhere else, and without it the next agent reads a pre-existing warning as a fresh regression.</rule>
    </body_rules>
  </pattern>

  <pattern name="memory_edit_hygiene">
    <description>Mechanical rules for editing an existing memory. These exist because each one has a recorded failure behind it, and each failure is silent — the edit reports success and the damage is only visible to a later reader.</description>
    <rules>
      <rule>Read the whole memory before editing it, not just the region being changed. Editing from a partial view is how a second complete frontmatter block ends up stacked on top of the first, after which a consumer parsing the first block and one parsing the last get different answers. This is the re-read-before-editing rule applied to memory rather than to source.</rule>
      <rule>After the edit, the file must contain exactly one frontmatter block. Check it rather than assuming it.</rule>
      <rule>Any programmatic in-place substitution whose replacement text contains a metacharacter must be verified by reading the result, not by the command's exit status. A replacement containing a capture-group reference with no corresponding group in the pattern substitutes the empty string silently and truncates the sentence; a replacement written through a layer that does not interpret escapes emits the escape sequence as literal characters. Neither fails loudly, and both destroy content in a file nobody will re-read.</rule>
    </rules>
  </pattern>

  <pattern name="memory_duplicate_detection">
    <description>How to find the existing entry before writing a new one. "Check list_memories first" stops working the moment the namespace is split by domain prefix, which is exactly when the corpus is large enough for duplication to matter.</description>
    <problem>Working inside one domain, the natural name for a new memory carries that domain's prefix, so the identical fact already filed under a different domain never comes into view. The bodies diverge in vocabulary too, so neither a name scan nor a full-text grep for the obvious term finds the existing entry. The observed cost is a fact recorded in seven places where the sum of the seven carries less information than the best single copy — each partial, and the decisive detail present in only one.</problem>
    <rules>
      <rule>Search by the words describing the symptom, not by the words you would use to name the file. "Tests run stale logic", "the edit did not take effect" — a reader hits the memory through the problem they are having, never through the taxonomy someone else chose.</rule>
      <rule>A fact that crosses domains goes in one cross-cutting place, not under whichever domain happened to hit it first.</rule>
      <rule>When you do find the duplicate, merge rather than adding another copy, and keep the detail that appears in only one of them. That detail is usually the reason the memory is worth having.</rule>
    </rules>
  </pattern>

  <pattern name="shared_active_project_pointer">
    <description>Serena's active-project pointer is shared, so concurrent sessions can move it out from under each other. A memory operation that fails or comes back short during a heavy parallel dispatch is usually a routing problem, not missing data.</description>
    <symptoms>
      <symptom>edit_memory returns a not-found error for a memory known to exist.</symptom>
      <symptom>list_memories returns a small, unrelated set instead of the expected corpus.</symptom>
    </symptoms>
    <recovery>
      <step order="1">Re-run activate_project with this session's own absolute project path.</step>
      <step order="2">Retry the memory operation. The files on disk were never touched; only the pointer moved.</step>
    </recovery>
    <rule>A subagent reporting "no relevant memory exists" or "list_memories returned only unrelated entries" during a parallel-worktree session may be hitting the same routing confusion. Do not treat that negative as authoritative — acting on it means writing a duplicate of an entry that already exists under the project the pointer drifted away from.</rule>
  </pattern>

  <pattern name="memory_lifecycle">
    <description>Memory versioning, archival, and consolidation patterns. Freshness is maintained lazily: memory_staleness_verification runs during normal task execution against only the memories that task happened to read. There is deliberately no automatic full-index sweep — a memory nothing has read in months is also a memory nothing has needed, and sweeping the whole index on a schedule costs more than the staleness it finds. Consolidating the index is a user-initiated activity, not a step any task performs on its own.</description>
    <versioning>
      <convention>Use date suffix for major updates: {name}-YYYY-MM</convention>
      <example>claude-code-architecture-2026-01</example>
      <note>For minor updates, use edit_memory instead of creating new version</note>
    </versioning>
    <archival>
      <trigger>When pattern is superseded by new approach</trigger>
      <action>Rename with -archived suffix using rename_memory OR delete if no historical value</action>
      <example>rename_memory "old-pattern" "old-pattern-archived"</example>
      <forward_pointer>A rename reaches only whoever consults the index next. It leaves nothing behind for the session that already loaded the old memory, and nothing for the other memories that cite it. So the superseding memory should name the old one and state what it wrongly claimed. Recording the wrong claim, not just the name, is what lets a reader match it against what they remember reading — a bare name leaves them at "I think I read that, but I cannot recall what it said".</forward_pointer>
      <keep_the_correction_visible>Where the old claim is likely to be re-derived from the same code, keep it visible with its correction and date rather than deleting it outright. A silently overwritten memory has a specific recurring cost: the next session re-derives the superseded claim, reaches the same wrong conclusion, and has no way to know it has been here before. This applies with particular force to a recorded rejection — "we evaluated this and decided against it" — which should be stored with its reasoning, not just its verdict, because a rejection goes stale when its premise is invalidated even though its conclusion still sounds right.</keep_the_correction_visible>
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
    <description>Lightweight, opportunistic freshness check applied only to memories actually read during a task. Piggybacks on read_memory calls already mandated by SERENA-B002, so it adds no extra memory reads of its own — which is the whole point: verification rides along with work that was happening anyway.</description>
    <trigger>A memory loaded via read_memory during this task was relied upon (its content informed a decision, an implementation choice, or an investigation finding)</trigger>
    <staleness_signal>
      <primary>Frontmatter last-verified field (see memory_content_format) — stale if more than 3 months old</primary>
      <fallback>If frontmatter is absent, treat the memory as a stale candidate on this basis alone and add frontmatter when editing (see memory_content_format rules)</fallback>
      <named_predicate>For a memory that names something checkable — a symbol, a file, a path, a condition — the test that matters is not how old it is but whether the thing it names still exists. Those two questions have different answers: a memory verified two months ago can name a symbol deleted last week and pass the date gate untouched. Checking costs one search, far less than the review round that re-derives the item and re-files it as an action. Apply this in particular to any memory that carries forward a deferred work item, because that item is re-proposed on the strength of a name that may no longer resolve.</named_predicate>
    </staleness_signal>
    <verification_is_a_comparison>
      <rule>Bumping last-verified is one edit; verifying content requires actually reading the code. If only the first happens, the stamp comes to mean "recently touched" rather than "checked", which is worse for a reader than no stamp at all — an unmarked memory invites suspicion, a freshly-dated one invites trust. State what was compared against what, naming a file path or a command output, whenever the date is bumped.</rule>
    </verification_is_a_comparison>
    <partial_verification>
      <description>The three outcomes above — still accurate, partially outdated, fully superseded — are all whole-file verdicts, and a task rarely touches a whole memory. Partial re-verification is the normal case, not the exception, so it needs its own form.</description>
      <rule>Record the boundary of what was checked, in the memory body, naming both sides. Bumping last-verified for the whole file after confirming one section lends false freshness to everything else in it; not bumping it at all sends the confirmed part back through verification next time. Neither is right, and the boundary statement is what makes the date honest.</rule>
      <rule>Record a discrepancy found during a scoped check at the moment it is found, even when it lies outside what the task set out to verify. Noting that a named function or path no longer exists costs a line and closes the finding; deferring it means an independent investigation later that starts from nothing.</rule>
    </partial_verification>
    <superseded_banner>
      <description>An alternative to the correct-in-place and archive-by-rename outcomes, for a memory whose value is mixed — some claims still hold, others are dead. Correcting in place erases the audit trail; archiving discards the parts that were confirmed.</description>
      <form>Keep the file. Put a dated banner at the top stating which claims are still true, and explicitly invalidating the point-in-time facts — scores, counts, line numbers — as historical rather than current. Where the split is substantial, divide the body into what was re-verified and what is retained as history.</form>
      <why>A later session quoting a stale number is stopped at the moment it opens the file, rather than after it has built on the number. The banner also gives the memory a status that a metadata field will not reliably carry, because the banner is in the text a reader actually loads.</why>
      <numbers_with_provenance>Where a figure genuinely must be recorded, write it as an observation made at a stated time by a stated command, never in the present tense as a current fact. "Reported N at &lt;date&gt; via &lt;command&gt;" ages honestly; "the suite contains N tests" does not, and a corpus accumulates several mutually contradictory values of N, each confident and each correct when written.</numbers_with_provenance>
    </superseded_banner>
    <action_by_outcome>
      <outcome name="still_accurate">edit_memory to bump last-verified to the current YYYY-MM; add frontmatter first if it was missing</outcome>
      <outcome name="partially_outdated">edit_memory to correct the stale section and bump last-verified</outcome>
      <outcome name="fully_superseded">rename_memory with an "-archived" suffix; note the reason in output</outcome>
    </action_by_outcome>
    <scope_boundary>
      <in_scope>Only memories the task already read for its own purposes</in_scope>
      <out_of_scope>Proactively reading additional memories solely to check their freshness. That turns every task into an index sweep, and the reads are charged to a task that never needed them</out_of_scope>
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
  <practice priority="high">Give a growing memory corpus a single root and describe what each reference covers, so memories are reached by traversal rather than by reading everything (memory_reference_graph)</practice>
  <practice priority="medium">When a parallel subagent's completion notification does not arrive, read its transcript before concluding it failed (parallel_subagent_result_recovery)</practice>
  <practice priority="critical">Keep a memory body describing the current state; an addition that invalidates existing text rewrites that text in the same edit, and a retraction goes in the opening line (memory_content_format body_rules)</practice>
  <practice priority="high">Store the command that produces a figure, never the figure itself, and record the exact command a later reader can re-run to re-establish the claim (memory_content_format body_rules)</practice>
  <practice priority="high">Read the whole memory before edit_memory, and confirm the result carries exactly one frontmatter block (memory_edit_hygiene)</practice>
  <practice priority="high">Search for an existing duplicate by the symptom wording rather than by the name you would give the file (memory_duplicate_detection)</practice>
  <practice priority="medium">For a memory that names a symbol or path, check that the name still resolves — that is a different question from how old the memory is (memory_staleness_verification)</practice>
  <practice priority="high">When re-verification is partial, write the boundary of what was checked into the body; a whole-file date bump after a section-level check lends false freshness to the rest (memory_staleness_verification)</practice>
  <practice priority="medium">During heavy parallel dispatch, re-activate the project before believing a memory lookup that came back empty (shared_active_project_pointer)</practice>
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
  <rule>Follow memory_staleness_verification for memories read during a task; never read a memory solely to check its freshness</rule>
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

<error_escalation>
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
