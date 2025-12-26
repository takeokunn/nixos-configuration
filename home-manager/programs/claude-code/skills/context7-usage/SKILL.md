---
name: Context7 Usage
description: This skill should be used when the user asks to "check documentation", "latest API", "library docs", "context7", or needs up-to-date library documentation. Provides Context7 MCP usage patterns.
version: 0.1.0
---

<purpose>
Provide patterns for effective use of Context7 MCP for retrieving up-to-date library documentation.
</purpose>

<tool_overview>
<tool name="resolve-library-id">
<description>Resolve package name to Context7-compatible library ID</description>
<parameters>
<param name="libraryName">Library name to search for</param>
</parameters>
<output>List of matching libraries with IDs, trust scores, snippet counts</output>
<required>Must call before get-library-docs unless ID is known</required>
</tool>

<tool name="get-library-docs">
<description>Fetch documentation for a specific library</description>
<parameters>
<param name="context7CompatibleLibraryID">Library ID from resolve-library-id</param>
<param name="topic">Optional topic to focus on (e.g., "hooks", "routing")</param>
<param name="tokens">Max tokens to retrieve (default: 5000)</param>
</parameters>
<output>Documentation snippets with code examples</output>
</tool>
</tool_overview>

<usage_workflow>
<step id="1" name="resolve">
Call resolve-library-id with library name.
<example>resolve-library-id libraryName="react"</example>
</step>

<step id="2" name="select">
Choose best match based on:
<criterion>Name similarity to query</criterion>
<criterion>Trust score (7-10 preferred)</criterion>
<criterion>Code snippet count (higher is better)</criterion>
<criterion>Description relevance</criterion>
</step>

<step id="3" name="fetch">
Call get-library-docs with selected ID.
<example>get-library-docs context7CompatibleLibraryID="/facebook/react" topic="hooks"</example>
</step>
</usage_workflow>

<common_libraries>
<library name="React" id="/facebook/react" />
<library name="Next.js" id="/vercel/next.js" />
<library name="TypeScript" id="/microsoft/typescript" />
<library name="Node.js" id="/nodejs/node" />
<library name="Express" id="/expressjs/express" />
<library name="NixOS/nixpkgs" id="/nixos/nixpkgs" />
<library name="Home Manager" id="/nix-community/home-manager" />
</common_libraries>

<topic_patterns>
<pattern name="specific_feature">
Use topic to narrow documentation focus.
<example>topic="authentication" for auth-related docs</example>
<example>topic="hooks" for React hooks documentation</example>
</pattern>

<pattern name="getting_started">
Omit topic for general overview.
</pattern>

<pattern name="api_reference">
Use topic="api" or specific API name.
<example>topic="useState" for specific hook</example>
</pattern>
</topic_patterns>

<token_guidance>
<recommendation name="quick_lookup" tokens="2000-3000">
For specific API or function documentation.
</recommendation>

<recommendation name="standard" tokens="5000">
Default for most queries.
</recommendation>

<recommendation name="comprehensive" tokens="8000-10000">
For complex topics requiring extensive context.
</recommendation>
</token_guidance>

<best_practices>
<practice>Always resolve library ID before fetching docs</practice>
<practice>Use specific topics to reduce token usage</practice>
<practice>Prefer high trust score libraries</practice>
<practice>Check snippet count for documentation quality</practice>
<practice>Use versioned IDs when available for specific versions</practice>
</best_practices>

<integration_with_serena>
<pattern name="verify_usage">
<step>Use Serena to find how library is currently used in codebase</step>
<step>Use Context7 to get latest documentation</step>
<step>Compare current usage with documented best practices</step>
</pattern>

<pattern name="update_dependencies">
<step>Use Context7 to check latest API changes</step>
<step>Use Serena to find all usages of changed APIs</step>
<step>Plan migration based on documentation</step>
</pattern>
</integration_with_serena>

<error_handling>
<case name="library_not_found">
Try alternative names or broader search terms.
<example>"react-query" vs "tanstack/query"</example>
</case>

<case name="low_snippet_count">
Consider using alternative library ID or broader topic.
</case>

<case name="version_mismatch">
Use versioned library ID format: /org/project/version
</case>
</error_handling>
