---
name: Serena Usage
description: This skill should be used when the user asks to "use serena", "semantic search", "symbol analysis", "find references", "code navigation", or needs Serena MCP guidance. Provides Serena tool usage patterns.
version: 0.1.0
---

<purpose>
Provide patterns for effective use of Serena MCP tools for semantic code operations.
</purpose>

<tools>
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
<use_case>Find patterns, non-code files, complex searches</use_case>
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
<use_case>Add imports, decorators, comments</use_case>
</tool>
<tool name="insert_after_symbol">
<description>Insert content after a symbol</description>
<use_case>Add new functions, classes</use_case>
</tool>
<tool name="rename_symbol">
<description>Rename symbol across codebase</description>
<use_case>Consistent renaming with reference updates</use_case>
</tool>
</tools>

<memory>
<tool name="list_memories">List available memory files; check existing patterns before implementation</tool>
<tool name="read_memory">Read content of a memory file; load conventions, patterns, decisions</tool>
<tool name="write_memory">Save information for future reference; record patterns, conventions, decisions</tool>
<tool name="delete_memory">Remove obsolete memory; clean up outdated information</tool>
<naming>
<pattern format="{project}-conventions">Project conventions</pattern>
<pattern format="{feature}-patterns">Feature patterns</pattern>
<pattern format="{domain}-patterns">Domain patterns</pattern>
<pattern format="architecture-{decision}">Architecture decisions</pattern>
</naming>
</memory>

<patterns>
<pattern name="explore_file">
<step>get_symbols_overview with depth=0 for top-level view</step>
<step>get_symbols_overview with depth=1 for class members</step>
<step>find_symbol with include_body=true for specific implementation</step>
</pattern>
<pattern name="trace_dependencies">
<step>find_symbol to locate the symbol</step>
<step>find_referencing_symbols to find all callers</step>
<step>Recursively trace for full dependency graph</step>
</pattern>
<pattern name="safe_refactoring">
<step>find_symbol to understand current implementation</step>
<step>find_referencing_symbols to identify all usages</step>
<step>replace_symbol_body for the change</step>
<step>Update references if interface changed</step>
</pattern>
<pattern name="pattern_search">
<step>search_for_pattern with restrict_search_to_code_files=true for code</step>
<step>search_for_pattern with restrict_search_to_code_files=false for all files</step>
<step>Use context_lines for surrounding code</step>
</pattern>
</patterns>

<rules>
<rule>Always check memories before implementing new features</rule>
<rule>Use symbol operations over reading entire files</rule>
<rule>Restrict searches by relative_path when scope is known</rule>
<rule>Use substring_matching for uncertain symbol names</rule>
<rule>Record new patterns with write_memory</rule>
</rules>

<thinking>
<tool name="think_about_collected_information">Call after non-trivial search sequences to evaluate sufficiency</tool>
<tool name="think_about_task_adherence">Call before making code changes to ensure alignment</tool>
<tool name="think_about_whether_you_are_done">Call when task appears complete to verify</tool>
</thinking>
