---
name: Serena Usage
description: This skill should be used when the user asks to "use serena", "semantic search", "symbol analysis", "find references", "code navigation", or needs Serena MCP guidance. Provides Serena tool usage patterns and orchestration integration.
version: 0.2.0
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
    <use_case>First step when exploring a new file</use_case>
  </tool>

  <tool name="find_symbol">
    <description>Find symbols by name path pattern</description>
    <param name="name_path_pattern">Symbol name or path (e.g., "MyClass/myMethod")</param>
    <param name="relative_path">Restrict to file or directory</param>
    <param name="include_body">Include source code</param>
    <param name="depth">Include descendants</param>
    <param name="substring_matching">Match partial names</param>
    <use_case>Locate specific functions, classes, or methods</use_case>
  </tool>

  <tool name="find_referencing_symbols">
    <description>Find all references to a symbol</description>
    <param name="name_path">Symbol to find references for</param>
    <param name="relative_path">File containing the symbol</param>
    <use_case>Dependency analysis, impact assessment</use_case>
  </tool>

  <tool name="search_for_pattern">
    <description>Regex search across codebase</description>
    <param name="substring_pattern">Regex pattern</param>
    <param name="relative_path">Restrict search scope</param>
    <param name="context_lines_before">Lines before match</param>
    <param name="context_lines_after">Lines after match</param>
    <param name="restrict_search_to_code_files">Search only code files or all files</param>
    <param name="paths_include_glob">Glob pattern for files to include</param>
    <param name="paths_exclude_glob">Glob pattern for files to exclude</param>
    <use_case>Find patterns, non-code files, complex searches</use_case>
  </tool>

  <tool name="list_dir">
    <description>List directory contents</description>
    <param name="relative_path">Directory path</param>
    <param name="recursive">Include subdirectories</param>
    <use_case>Explore directory structure</use_case>
  </tool>

  <tool name="find_file">
    <description>Find files by name pattern</description>
    <param name="file_name_pattern">File name pattern to search</param>
    <param name="relative_path">Restrict search scope</param>
    <use_case>Locate files by name</use_case>
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
    <use_case>Add imports, decorators, comments</use_case>
  </tool>

  <tool name="insert_after_symbol">
    <description>Insert content after a symbol</description>
    <param name="name_path">Symbol to insert after</param>
    <param name="relative_path">File containing symbol</param>
    <param name="body">Content to insert</param>
    <use_case>Add new functions, classes</use_case>
  </tool>

  <tool name="rename_symbol">
    <description>Rename symbol across codebase</description>
    <param name="name_path">Symbol to rename</param>
    <param name="relative_path">File containing symbol</param>
    <param name="new_name">New symbol name</param>
    <use_case>Consistent renaming with reference updates</use_case>
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
    <description>Write information to a memory file</description>
    <param name="memory_file_name">Name of memory file to write</param>
    <param name="content">Content to write</param>
    <use_case>Record new patterns and conventions</use_case>
  </tool>

  <tool name="edit_memory">
    <description>Replace content in a memory file using regex or literal matching</description>
    <param name="memory_file_name">Name of memory file to edit</param>
    <param name="needle">Pattern to search for</param>
    <param name="repl">Replacement string</param>
    <param name="mode">Either "literal" or "regex"</param>
    <use_case>Update specific parts of memory files</use_case>
  </tool>

  <tool name="delete_memory">
    <description>Delete a memory file</description>
    <param name="memory_file_name">Name of memory file to delete</param>
    <use_case>Remove obsolete or incorrect patterns (requires user permission)</use_case>
  </tool>

  <tool name="think_about_collected_information">
    <description>Validate search quality and completeness</description>
    <use_case>After investigation or search sequences</use_case>
  </tool>

  <tool name="think_about_task_adherence">
    <description>Verify alignment with task before code modification</description>
    <use_case>Before any symbol editing operation</use_case>
  </tool>

  <tool name="think_about_whether_you_are_done">
    <description>Confirm task completion</description>
    <use_case>Before reporting task completion to user</use_case>
  </tool>
</tools>

<concepts>
  <concept name="memory_files">
    <description>Persistent storage for project patterns, conventions, and architectural decisions</description>
    <example>
      list_memories  # Check existing patterns
      read_memory "nix-conventions"  # Load Nix patterns
      write_memory "api-patterns" "REST API conventions..."  # Record new pattern
      edit_memory "api-patterns" "old text" "new text" mode="literal"  # Update memory
    </example>
  </concept>

  <concept name="memory_naming">
    <description>Consistent naming conventions for memory files</description>
    <example>
      {project}-conventions  # Project-wide conventions
      {feature}-patterns     # Feature-specific patterns
      {domain}-patterns      # Domain-specific patterns
      architecture-{decision}  # Architecture decisions
    </example>
  </concept>

  <concept name="symbol_path">
    <description>Path to symbol within a file (e.g., MyClass/myMethod)</description>
    <example>
      find_symbol "MyClass/myMethod"  # Find specific method
      find_symbol "get*" substring_matching=true  # Find all getter methods
    </example>
  </concept>

  <concept name="reflection_tools">
    <description>Meta-cognitive tools for ensuring search quality and task alignment</description>
    <example>
      <note>After searching multiple files</note>
      think_about_collected_information

      <note>Before making changes</note>
      think_about_task_adherence

      <note>When task seems complete</note>
      think_about_whether_you_are_done
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="session_initialization">
    <description>Initialize Serena at session start</description>
    <example>
      <step>Step 1: Activate project</step>
      activate_project project="project-name"

      <step>Step 2: Verify onboarding</step>
      check_onboarding_performed

      <step>Step 3: If not onboarded, run onboarding</step>
      onboarding (if needed)

      <step>Step 4: Check available memories</step>
      list_memories
    </example>
  </pattern>

  <pattern name="explore_file">
    <description>Systematically explore file structure from high-level to detailed</description>
    <example>
      <step>Step 1: Get top-level overview</step>
      get_symbols_overview relative_path="src/main.ts" depth=0

      <step>Step 2: Explore class members</step>
      get_symbols_overview relative_path="src/main.ts" depth=1

      <step>Step 3: Get specific implementation</step>
      find_symbol name_path_pattern="MyClass/myMethod" include_body=true
    </example>
  </pattern>

  <pattern name="trace_dependencies">
    <description>Trace symbol dependencies and callers</description>
    <example>
      <step>Step 1: Locate the symbol</step>
      find_symbol name_path_pattern="processData" relative_path="src/processor.ts"

      <step>Step 2: Find all callers</step>
      find_referencing_symbols name_path="processData" relative_path="src/processor.ts"

      <step>Step 3: Recursively trace for full dependency graph</step>
      <note>Repeat steps 1-2 for each caller to build complete dependency tree</note>
    </example>
  </pattern>

  <pattern name="safe_refactoring">
    <description>Refactor with full impact analysis</description>
    <example>
      <step>Step 1: Understand current implementation</step>
      find_symbol name_path_pattern="MyClass/oldMethod" include_body=true

      <step>Step 2: Identify all usages</step>
      find_referencing_symbols name_path="MyClass/oldMethod" relative_path="src/myclass.ts"

      <step>Step 3: Call think_about_task_adherence</step>
      think_about_task_adherence

      <step>Step 4: Perform the change</step>
      replace_symbol_body name_path="MyClass/oldMethod" relative_path="src/myclass.ts" body="..."

      <step>Step 5: Update references if interface changed</step>
      <note>If method signature changed, update all calling sites identified in step 2</note>
    </example>
  </pattern>

  <pattern name="pattern_search">
    <description>Search for patterns with appropriate scope</description>
    <example>
      <note>Search only in code files</note>
      search_for_pattern substring_pattern="TODO:" restrict_search_to_code_files=true

      <note>Search in all files including configs</note>
      search_for_pattern substring_pattern="api-key" restrict_search_to_code_files=false

      <note>Include context around matches</note>
      search_for_pattern substring_pattern="function.\*async" context_lines_before=2 context_lines_after=3
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

  <pattern name="memory_lifecycle">
    <description>Memory versioning, archival, and consolidation patterns</description>
    <versioning>
      <convention>Use date suffix for major updates: {name}-YYYY-MM</convention>
      <example>claude-code-architecture-2026-01</example>
      <note>For minor updates, use edit_memory instead of creating new version</note>
    </versioning>
    <archival>
      <trigger>When pattern is superseded by new approach</trigger>
      <action>Rename with -archived suffix OR delete if no historical value</action>
      <example>edit_memory "old-pattern" "..." "ARCHIVED: Superseded by new-pattern\n\n..."</example>
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

  <decision_tree name="serena_code_operation">
    <question>What type of code operation is needed?</question>
    <branch condition="Understand file structure">Use get_symbols_overview with depth=1</branch>
    <branch condition="Find specific symbol by name">Use find_symbol with name_path_pattern</branch>
    <branch condition="Read symbol implementation">Use find_symbol with include_body=true</branch>
    <branch condition="Find symbol references">Use find_referencing_symbols</branch>
    <branch condition="Replace entire symbol body">Use replace_symbol_body</branch>
    <branch condition="Add new code after symbol">Use insert_after_symbol</branch>
    <branch condition="Add new code before symbol">Use insert_before_symbol</branch>
    <branch condition="Rename symbol across codebase">Use rename_symbol</branch>
    <branch condition="Search pattern in non-code files">Use search_for_pattern</branch>
    <branch condition="List directory contents">Use list_dir</branch>
    <branch condition="Find files by name">Use find_file</branch>
  </decision_tree>

  <decision_tree name="tool_selection">
    <question>What type of operation is needed?</question>
    <branch condition="Symbol lookup">Use find_symbol with name_path_pattern</branch>
    <branch condition="File structure overview">Use get_symbols_overview with appropriate depth</branch>
    <branch condition="Find references">Use find_referencing_symbols</branch>
    <branch condition="Pattern search">Use search_for_pattern with regex</branch>
    <branch condition="Refactor symbol">Use replace_symbol_body</branch>
    <branch condition="Rename symbol">Use rename_symbol for consistent updates</branch>
    <branch condition="Add code">Use insert_before_symbol or insert_after_symbol</branch>
    <branch condition="Check patterns">Use list_memories then read_memory</branch>
    <branch condition="Record patterns">Use write_memory</branch>
    <branch condition="Update patterns">Use edit_memory</branch>
  </decision_tree>

  <decision_tree name="serena_first_tool_selection">
    <description>Serena-first tool selection hierarchy with explicit fallback conditions</description>
    <question>What type of search or code operation is needed?</question>
    <branch condition="Find files by name pattern">
      <primary>Use Serena find_file</primary>
      <fallback condition="Serena project not activated OR find_file returns no results">Use Glob</fallback>
    </branch>
    <branch condition="Search file contents for pattern">
      <primary>Use Serena search_for_pattern</primary>
      <fallback condition="Serena project not activated OR non-code files need different treatment">Use Grep</fallback>
    </branch>
    <branch condition="List directory contents">
      <primary>Use Serena list_dir</primary>
      <fallback condition="Serena project not activated">Use Bash ls</fallback>
    </branch>
    <branch condition="View file structure/symbols">
      <primary>Use Serena get_symbols_overview (depth=0 first, then depth=1)</primary>
      <fallback condition="File is not code (YAML, JSON, MD) OR Serena unavailable">Use Read with offset/limit</fallback>
    </branch>
    <branch condition="Find specific function/class/method">
      <primary>Use Serena find_symbol with name_path_pattern</primary>
      <fallback condition="Symbol name unknown">Use find_symbol with substring_matching=true</fallback>
      <fallback condition="Still no results OR non-code file">Use Grep</fallback>
    </branch>
    <branch condition="Understand code in file">
      <primary>Use Serena get_symbols_overview then find_symbol with include_body=true</primary>
      <fallback condition="Need full file context OR non-code file">Use Read</fallback>
    </branch>
    <branch condition="Find all usages of symbol">
      <primary>Use Serena find_referencing_symbols</primary>
      <fallback condition="Symbol not in LSP scope">Use Grep with symbol name pattern</fallback>
    </branch>
    <unavailable_conditions>
      <condition>Serena project not activated (call activate_project first)</condition>
      <condition>File type not supported by LSP (use search_for_pattern or Read)</condition>
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
      <preference>Use search_for_pattern for structured search</preference>
      <fallback>Use Read for full context when structure is complex</fallback>
    </branch>
    <branch condition="Markup/Documentation (Markdown, RST, HTML)">
      <preference>Use search_for_pattern or Read</preference>
      <reason>Symbol operations less useful for prose</reason>
    </branch>
  </decision_tree>
</patterns>

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

  <avoid name="skipping_reflection">
    <description>Modifying code without calling think_about_task_adherence</description>
    <instead>Always call think_about_task_adherence before symbol editing operations</instead>
  </avoid>

  <avoid name="deleting_memories_without_permission">
    <description>Using delete_memory without explicit user request</description>
    <instead>Use edit_memory to update memories; only delete when user explicitly requests</instead>
  </avoid>

  <avoid name="using_glob_grep_directly">
    <description>Using Glob/Grep tools when Serena navigation tools are available</description>
    <instead>Prefer Serena list_dir, find_file, search_for_pattern for codebase exploration</instead>
  </avoid>
</anti_patterns>

<workflow>
  <phase name="prepare">
    <objective>Prepare for effective Serena tool usage</objective>
    <step>1. Activate project with activate_project</step>
    <step>2. Verify onboarding with check_onboarding_performed</step>
    <step>3. Check list_memories for existing patterns</step>
    <step>4. Read relevant memories with read_memory</step>
    <step>5. Identify target symbols or files</step>
    <step>6. Choose appropriate tool based on decision_tree</step>
  </phase>
  <phase name="execute">
    <objective>Execute Serena operations efficiently</objective>
    <step>1. Start with get_symbols_overview for file structure</step>
    <step>2. Use find_symbol with include_body for details</step>
    <step>3. Use find_referencing_symbols for dependencies</step>
    <step>4. Call think_about_task_adherence before edits</step>
    <step>5. Use symbol editing tools for modifications</step>
  </phase>
  <phase name="verify">
    <objective>Verify results and record patterns</objective>
    <step>1. Call think_about_collected_information after searches</step>
    <step>2. Record new patterns with write_memory</step>
    <step>3. Call think_about_whether_you_are_done when complete</step>
  </phase>
</workflow>

<best_practices>
  <practice priority="critical">Always activate project and check onboarding at session start (SERENA-B001)</practice>
  <practice priority="critical">Always check memories with list_memories and read_memory before implementing new features (SERENA-B002)</practice>
  <practice priority="critical">Use symbol operations (get_symbols_overview, find_symbol) over reading entire files (SERENA-B007)</practice>
  <practice priority="critical">Call think_about_task_adherence before any code modification (SERENA-B003)</practice>
  <practice priority="critical">Call think_about_collected_information after 3+ search operations (SERENA-B006)</practice>
  <practice priority="critical">Call think_about_whether_you_are_done before final response (SERENA-B004)</practice>
  <practice priority="critical">Use Serena navigation tools before Glob/Grep/Read (SERENA-P004)</practice>
  <practice priority="high">Restrict searches by relative_path when scope is known to improve performance</practice>
  <practice priority="high">Use substring_matching for uncertain symbol names to broaden search results</practice>
  <practice priority="high">Record significant patterns with write_memory after discovery (SERENA-B005)</practice>
  <practice priority="high">Use edit_memory for updating existing memories instead of delete and recreate</practice>
  <practice priority="high">Follow serena_first_tool_selection decision tree for consistent tool choices</practice>
  <practice priority="medium">Use context_lines parameters in search_for_pattern to understand surrounding code</practice>
  <practice priority="medium">Verify symbol changes with find_referencing_symbols before refactoring</practice>
  <practice priority="medium">Follow memory_reading_by_task_type to prioritize relevant memories</practice>
</best_practices>

<rules priority="critical">
  <rule>Always check memories before implementing new features (SERENA-B002)</rule>
  <rule>Use symbol operations (get_symbols_overview, find_symbol) over reading entire files (SERENA-B007, SERENA-P001)</rule>
  <rule>Call think_about_collected_information after search sequences of 3+ operations (SERENA-B006)</rule>
  <rule>Call think_about_task_adherence before making code changes (SERENA-B003)</rule>
  <rule>Call think_about_whether_you_are_done before final response (SERENA-B004, SERENA-P005)</rule>
  <rule>Use Serena navigation tools (find_file, search_for_pattern, list_dir) before Glob/Grep/Read (SERENA-P004)</rule>
  <rule>Use Serena symbol editing (replace_symbol_body, insert_after_symbol, insert_before_symbol, rename_symbol) for precise code modifications</rule>
  <rule>Record significant patterns with write_memory after discovery (SERENA-B005, SERENA-P007)</rule>
</rules>

<rules priority="standard">
  <rule>Restrict searches by relative_path when scope is known</rule>
  <rule>Use substring_matching for uncertain symbol names</rule>
  <rule>Use edit_memory for updating existing memories; delete_memory only when explicitly requested by user</rule>
  <rule>Follow serena_first_tool_selection decision tree for tool choices</rule>
  <rule>Follow language_specific_symbol_operations for language-appropriate tools</rule>
  <rule>Follow memory_reading_by_task_type for prioritizing which memories to read</rule>
</rules>

<enforcement>
  <mandatory_behaviors>
    <behavior id="SERENA-B001" priority="critical">
      <trigger>Session start</trigger>
      <action>Activate project with activate_project and check_onboarding_performed</action>
      <verification>Project activation recorded in output</verification>
    </behavior>
    <behavior id="SERENA-B002" priority="critical">
      <trigger>Before any implementation</trigger>
      <action>Check Serena memories with list_memories</action>
      <verification>Memory check recorded in output</verification>
    </behavior>
    <behavior id="SERENA-B003" priority="critical">
      <trigger>Before symbol editing</trigger>
      <action>Call think_about_task_adherence</action>
      <verification>Task adherence check recorded</verification>
    </behavior>
    <behavior id="SERENA-B004" priority="critical">
      <trigger>Before reporting completion</trigger>
      <action>Call think_about_whether_you_are_done</action>
      <verification>Completion check recorded</verification>
    </behavior>
    <behavior id="SERENA-B005" priority="critical">
      <trigger>After significant pattern discovery or successful implementation</trigger>
      <action>Create or update memory with write_memory or edit_memory</action>
      <verification>Memory operation recorded in output for reusable patterns</verification>
      <guidance>See memory_auto_creation_triggers pattern for when to create</guidance>
    </behavior>
    <behavior id="SERENA-B006" priority="critical">
      <trigger>After search sequence of 3+ operations (find_symbol, search_for_pattern, get_symbols_overview, Grep, Glob)</trigger>
      <action>Call think_about_collected_information</action>
      <verification>Search quality validation recorded</verification>
    </behavior>
    <behavior id="SERENA-B007" priority="critical">
      <trigger>When examining code structure or navigating codebase</trigger>
      <action>Use get_symbols_overview or find_symbol before Read for code files</action>
      <verification>Symbol operations attempted before full file read</verification>
      <exception>Non-code files (YAML, JSON, MD) may use Read directly</exception>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="SERENA-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Reading entire files when symbol operations suffice</action>
      <response>Use get_symbols_overview and find_symbol instead</response>
    </behavior>
    <behavior id="SERENA-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying code without think_about_task_adherence</action>
      <response>Call think_about_task_adherence first</response>
    </behavior>
    <behavior id="SERENA-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Deleting memories without explicit user request</action>
      <response>Use edit_memory instead or ask user permission</response>
    </behavior>
    <behavior id="SERENA-P004" priority="critical">
      <trigger>Always</trigger>
      <action>Using Glob/Grep/Read when Serena navigation tools available</action>
      <response>Use Serena list_dir, find_file, search_for_pattern, get_symbols_overview, find_symbol first</response>
      <fallback>Glob/Grep/Read allowed only when: Serena project not activated, tool explicitly fails, or file type not supported</fallback>
    </behavior>
    <behavior id="SERENA-P005" priority="critical">
      <trigger>Before final response to user</trigger>
      <action>Completing task without calling think_about_whether_you_are_done</action>
      <response>Call think_about_whether_you_are_done before final response</response>
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

<error_escalation>
  <level severity="low">
    <example>Symbol not found with exact match</example>
    <action>Retry with substring_matching=true, note in report, proceed</action>
  </level>
  <level severity="medium">
    <example>Memory file not found</example>
    <action>Document issue, use AskUserQuestion for clarification</action>
  </level>
  <level severity="high">
    <example>Conflicting information in memories</example>
    <action>STOP, present options to user</action>
  </level>
  <level severity="critical">
    <example>Memory corruption or invalid state</example>
    <action>BLOCK operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_skills>
  <skill name="investigation-patterns">Investigation methodology using Serena tools</skill>
  <skill name="nix-ecosystem">Nix patterns stored in Serena memories</skill>
  <skill name="typescript-ecosystem">TypeScript patterns stored in Serena memories</skill>
  <skill name="golang-ecosystem">Go patterns stored in Serena memories</skill>
  <skill name="rust-ecosystem">Rust patterns stored in Serena memories</skill>
  <skill name="common-lisp-ecosystem">Common Lisp patterns stored in Serena memories</skill>
  <skill name="emacs-ecosystem">Emacs patterns stored in Serena memories</skill>
</related_skills>

<constraints>
  <must>Activate project and check onboarding at session start (SERENA-B001)</must>
  <must>Check memories before implementing new features (SERENA-B002)</must>
  <must>Use symbol operations over reading entire files for code (SERENA-B007)</must>
  <must>Call think_about_task_adherence before code modifications (SERENA-B003)</must>
  <must>Call think_about_collected_information after 3+ search operations (SERENA-B006)</must>
  <must>Call think_about_whether_you_are_done before final response (SERENA-B004)</must>
  <must>Use Serena navigation tools before Glob/Grep/Read (SERENA-P004)</must>
  <must>Record significant patterns with write_memory (SERENA-B005)</must>
  <must>Restrict searches by relative_path when scope is known</must>
  <avoid>Reading entire files when symbol operations suffice (SERENA-P001)</avoid>
  <avoid>Unscoped searches across entire codebase</avoid>
  <avoid>Ignoring existing memory patterns (SERENA-B002 violation)</avoid>
  <avoid>Deleting memories without explicit user request (SERENA-P003)</avoid>
  <avoid>Using Glob/Grep/Read when Serena navigation tools are available (SERENA-P004)</avoid>
  <avoid>Skipping reflection checkpoints at mandatory points (SERENA-P005)</avoid>
  <avoid>Failing to document reusable patterns in memory (SERENA-P007)</avoid>
</constraints>
