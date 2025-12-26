---
name: Context7 Usage
description: This skill should be used when the user asks to "check documentation", "latest API", "library docs", "context7", or needs up-to-date library documentation. Provides Context7 MCP usage patterns.
version: 0.1.0
---

<purpose>
Provide patterns for effective use of Context7 MCP for retrieving up-to-date library documentation.
</purpose>

<tools>
<tool name="resolve-library-id">
<description>Resolve package name to Context7-compatible library ID</description>
<param name="libraryName">Library name to search for</param>
<output>List of matching libraries with IDs, trust scores, snippet counts</output>
<note>Must call before get-library-docs unless ID is known</note>
</tool>
<tool name="get-library-docs">
<description>Fetch documentation for a specific library</description>
<param name="context7CompatibleLibraryID">Library ID from resolve-library-id</param>
<param name="topic">Optional topic to focus on (e.g., "hooks", "routing")</param>
<param name="tokens">Max tokens to retrieve (default: 5000)</param>
<output>Documentation snippets with code examples</output>
</tool>
</tools>

<workflow>
<phase name="resolve">
<step>Call resolve-library-id with library name</step>
<example>resolve-library-id libraryName="react"</example>
</phase>
<phase name="select">
<criterion>Name similarity to query</criterion>
<criterion>Trust score (7-10 preferred)</criterion>
<criterion>Code snippet count (higher is better)</criterion>
<criterion>Description relevance</criterion>
</phase>
<phase name="fetch">
<step>Call get-library-docs with selected ID</step>
<example>get-library-docs context7CompatibleLibraryID="/facebook/react" topic="hooks"</example>
</phase>
</workflow>

<libraries>
<library name="React" id="/facebook/react" />
<library name="Next.js" id="/vercel/next.js" />
<library name="TypeScript" id="/microsoft/typescript" />
<library name="Node.js" id="/nodejs/node" />
<library name="Express" id="/expressjs/express" />
<library name="NixOS/nixpkgs" id="/nixos/nixpkgs" />
<library name="Home Manager" id="/nix-community/home-manager" />
</libraries>

<patterns>
<pattern name="specific_feature">
<description>Use topic to narrow documentation focus</description>
<example>topic="authentication" for auth-related docs</example>
<example>topic="hooks" for React hooks documentation</example>
</pattern>
<pattern name="getting_started">Omit topic for general overview</pattern>
<pattern name="api_reference">
<description>Use topic="api" or specific API name</description>
<example>topic="useState" for specific hook</example>
</pattern>
</patterns>

<tokens>
<recommendation name="quick_lookup" tokens="2000-3000">For specific API or function documentation</recommendation>
<recommendation name="standard" tokens="5000">Default for most queries</recommendation>
<recommendation name="comprehensive" tokens="8000-10000">For complex topics requiring extensive context</recommendation>
</tokens>

<rules>
<rule>Always resolve library ID before fetching docs</rule>
<rule>Use specific topics to reduce token usage</rule>
<rule>Prefer high trust score libraries</rule>
<rule>Check snippet count for documentation quality</rule>
<rule>Use versioned IDs when available for specific versions</rule>
</rules>

<integration>
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
</integration>

<errors>
<case name="library_not_found">Try alternative names or broader search terms (e.g., "react-query" vs "tanstack/query")</case>
<case name="low_snippet_count">Consider using alternative library ID or broader topic</case>
<case name="version_mismatch">Use versioned library ID format: /org/project/version</case>
</errors>
