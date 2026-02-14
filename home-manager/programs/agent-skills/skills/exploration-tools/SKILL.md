---
name: Exploration Tools
description: Tool definitions and usage patterns for codebase exploration (Glob, Grep, Read, LSP). Agents reference this skill instead of inline tool definitions.
version: 1.0.0
---

<purpose>
  Provide standardized tool definitions and usage patterns for codebase exploration. This skill centralizes tool knowledge that was previously duplicated across agents.
</purpose>

<tools>
  <tool name="Glob">
    <description>Fast file pattern matching for finding files by name</description>
    <params>
      <param name="pattern" required="true">Glob pattern (e.g., **/*.ts, src/**/*.md)</param>
      <param name="path" required="false">Directory to search in (defaults to cwd)</param>
    </params>
    <use_case>Find files by name pattern when you know the file extension or naming convention</use_case>
    <example>
      Pattern: **/*.tsx
      Result: All TypeScript React files in the project
    </example>
  </tool>

  <tool name="Grep">
    <description>Content search with regex support across files</description>
    <params>
      <param name="pattern" required="true">Regex pattern to search for</param>
      <param name="path" required="false">File or directory to search in</param>
      <param name="glob" required="false">Filter files by glob pattern</param>
      <param name="type" required="false">File type filter (js, py, rust, etc.)</param>
      <param name="output_mode" required="false">content, files_with_matches, or count</param>
      <param name="-A" required="false">Lines after match</param>
      <param name="-B" required="false">Lines before match</param>
      <param name="-C" required="false">Lines around match</param>
    </params>
    <use_case>Search file contents for patterns, keywords, or code constructs</use_case>
    <example>
      Pattern: useState
      Glob: **/*.tsx
      Result: All useState usages in React components
    </example>
  </tool>

  <tool name="Read">
    <description>Read file contents with optional line range</description>
    <params>
      <param name="file_path" required="true">Absolute path to file</param>
      <param name="offset" required="false">Starting line number</param>
      <param name="limit" required="false">Number of lines to read</param>
    </params>
    <use_case>View file contents after finding with Glob or Grep</use_case>
    <example>
      file_path: /path/to/file.ts
      offset: 50
      limit: 30
      Result: Lines 50-80 of the file
    </example>
  </tool>

  <tool name="LSP_goToDefinition">
    <description>Navigate to symbol definition using Language Server Protocol</description>
    <params>
      <param name="file" required="true">File containing the symbol reference</param>
      <param name="line" required="true">Line number of the reference</param>
      <param name="character" required="true">Column position of the reference</param>
    </params>
    <use_case>Find where a function, class, or variable is defined</use_case>
  </tool>

  <tool name="LSP_findReferences">
    <description>Find all references to a symbol using Language Server Protocol</description>
    <params>
      <param name="file" required="true">File containing the symbol definition</param>
      <param name="line" required="true">Line number of the definition</param>
      <param name="character" required="true">Column position of the definition</param>
    </params>
    <use_case>Find all usages of a function, class, or variable</use_case>
  </tool>

  <tool name="LSP_documentSymbol">
    <description>Get all symbols in a document using Language Server Protocol</description>
    <params>
      <param name="file" required="true">File to analyze</param>
    </params>
    <use_case>Get an overview of classes, functions, and variables in a file</use_case>
  </tool>
</tools>

<patterns>
  <pattern name="file_discovery">
    <description>Pattern for finding files by name or extension</description>
    <example>
      1. Use Glob with pattern to find candidate files
      2. Filter results by relevance
      3. Read specific files for details
    </example>
  </pattern>

  <pattern name="content_search">
    <description>Pattern for searching file contents</description>
    <example>
      1. Use Grep with pattern and file type filter
      2. Review matches with context (-C flag)
      3. Follow up with Read for full file context
    </example>
  </pattern>

  <pattern name="symbol_navigation">
    <description>Pattern for navigating code symbols</description>
    <example>
      1. Use LSP_goToDefinition to find symbol source
      2. Use LSP_findReferences to find usages
      3. Use LSP_documentSymbol for file overview
    </example>
  </pattern>
</patterns>

<concepts>
  <concept name="search_scope">
    <description>Choosing appropriate search boundaries</description>
    <example>
      Narrow: Single file or directory
      Medium: File type across project
      Broad: All files in project

      Start narrow, expand if needed
    </example>
  </concept>

  <concept name="result_ranking">
    <description>Prioritizing search results by relevance</description>
    <example>
      High relevance: Exact matches, definition sites
      Medium relevance: Usage sites, related patterns
      Low relevance: Comments, test files, generated code
    </example>
  </concept>
</concepts>

<best_practices>
  <practice priority="critical">Use Glob for file discovery, Grep for content search</practice>
  <practice priority="critical">Always return file:line references for findings</practice>
  <practice priority="high">Start with narrow search scope, expand if needed</practice>
  <practice priority="high">Use LSP tools when available for accurate symbol navigation</practice>
  <practice priority="medium">Filter out binary and generated files</practice>
  <practice priority="medium">Limit results to manageable size</practice>
</best_practices>

<anti_patterns>
  <avoid name="blind_broad_search">
    <description>Searching entire codebase without filters</description>
    <instead>Start with file type or directory filters</instead>
  </avoid>

  <avoid name="reading_without_searching">
    <description>Reading files without first using Glob/Grep to locate</description>
    <instead>Use search tools to find relevant files first</instead>
  </avoid>

  <avoid name="ignoring_context">
    <description>Returning matches without surrounding context</description>
    <instead>Use -C flag with Grep or read surrounding lines</instead>
  </avoid>
</anti_patterns>

<decision_tree name="tool_selection">
  <question>What type of search is needed?</question>
  <branch condition="Find files by name pattern">Use Glob</branch>
  <branch condition="Search file contents">Use Grep</branch>
  <branch condition="Find symbol definition">Use LSP_goToDefinition or Serena find_symbol</branch>
  <branch condition="Find symbol usages">Use LSP_findReferences or Serena find_referencing_symbols</branch>
  <branch condition="View file contents">Use Read</branch>
</decision_tree>

<constraints>
  <must>Return file paths with line numbers for all findings</must>
  <must>Limit results to manageable size</must>
  <must>Maintain read-only operations</must>
  <avoid>Modifying files during exploration</avoid>
  <avoid>Returning raw dumps without filtering</avoid>
  <avoid>Searching binary or generated files</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Alternative symbol navigation via Serena MCP</skill>
  <skill name="investigation-patterns">Evidence collection methodology using these tools</skill>
</related_skills>
