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
<use_case>Must call before get-library-docs unless ID is known</use_case>
<note>Returns list of matching libraries with IDs, trust scores, snippet counts</note>
</tool>

<tool name="get-library-docs">
<description>Fetch documentation for a specific library</description>
<param name="context7CompatibleLibraryID">Library ID from resolve-library-id</param>
<param name="topic">Optional topic to focus on (e.g., "hooks", "routing")</param>
<param name="tokens">Max tokens to retrieve (default: 5000)</param>
<use_case>Retrieve up-to-date documentation snippets with code examples</use_case>
</tool>
</tools>

<workflow>
<phase name="resolve">
<step>Call resolve-library-id with library name</step>
<example>
resolve-library-id libraryName="react"
</example>
</phase>

<phase name="select">
<criterion>Name similarity to query</criterion>
<criterion>Trust score (7-10 preferred)</criterion>
<criterion>Code snippet count (higher is better)</criterion>
<criterion>Description relevance</criterion>
</phase>

<phase name="fetch">
<step>Call get-library-docs with selected ID</step>
<example>
get-library-docs context7CompatibleLibraryID="/facebook/react" topic="hooks"
</example>
</phase>
</workflow>

<concept name="library_identifiers">
<description>Common Context7-compatible library IDs for frequently used packages</description>
<example>
React: /facebook/react
Next.js: /vercel/next.js
TypeScript: /microsoft/typescript
Node.js: /nodejs/node
Express: /expressjs/express
NixOS/nixpkgs: /nixos/nixpkgs
Home Manager: /nix-community/home-manager
</example>
</concept>

<concept name="token_allocation">
<description>Recommended token limits based on query type</description>
<example>
Quick lookup (specific API/function): 2000-3000 tokens
Standard queries: 5000 tokens (default)
Comprehensive topics: 8000-10000 tokens
</example>
</concept>

<pattern name="specific_feature">
<description>Use topic parameter to narrow documentation focus and reduce token usage</description>
<example>
<note>For authentication-related documentation</note>
topic="authentication"

<note>For React hooks documentation</note>

topic="hooks"

<note>For routing documentation</note>

topic="routing"
</example>
</pattern>

<pattern name="getting_started">
<description>Omit topic parameter for general overview and getting started documentation</description>
<example>
get-library-docs context7CompatibleLibraryID="/facebook/react"
</example>
</pattern>

<pattern name="api_reference">
<description>Use topic parameter with specific API names for focused reference documentation</description>
<example>
<note>For specific React hook</note>
topic="useState"

<note>For specific Next.js API</note>

topic="getServerSideProps"

<note>For specific TypeScript utility type</note>

topic="Partial"
</example>
</pattern>

<pattern name="verify_usage">
<description>Verify codebase usage against latest documentation by combining Serena and Context7</description>
<example>
<step>1. Use Serena to find current library usage in codebase</step>
find_symbol name_path_pattern="useState"

<step>2. Use Context7 to get latest documentation</step>

get-library-docs context7CompatibleLibraryID="/facebook/react" topic="useState"

<step>3. Compare current usage with documented best practices</step>

</example>
</pattern>

<pattern name="update_dependencies">
<description>Plan dependency updates with API migration by checking latest documentation</description>
<example>
<step>1. Use Context7 to check latest API changes</step>
get-library-docs context7CompatibleLibraryID="/vercel/next.js" topic="migration"

<step>2. Use Serena to find all usages of changed APIs</step>

find_referencing_symbols name_path="getStaticProps"

<step>3. Plan migration based on documentation</step>

</example>
</pattern>

<best_practices>
<practice priority="critical">Always resolve library ID before fetching documentation</practice>
<practice priority="critical">Prefer libraries with trust scores 7-10 for better documentation quality</practice>
<practice priority="high">Use specific topics to reduce token usage and increase relevance</practice>
<practice priority="high">Check snippet count as indicator of documentation completeness</practice>
<practice priority="medium">Use versioned IDs when available for specific version documentation</practice>
</best_practices>

<anti_patterns>
<avoid name="skipping_resolution">
<description>Calling get-library-docs without resolving library ID first</description>
<instead>Always call resolve-library-id to get the correct Context7-compatible library ID</instead>
</avoid>

<avoid name="excessive_tokens">
<description>Requesting maximum tokens for simple queries</description>
<instead>Use specific topics and appropriate token limits (2000-3000 for quick lookups, 5000 for standard queries)</instead>
</avoid>

<avoid name="ignoring_trust_scores">
<description>Using libraries with low trust scores or snippet counts</description>
<instead>Prefer libraries with trust scores 7-10 and higher snippet counts for better documentation quality</instead>
</avoid>

<avoid name="wrong_library_name">
<description>Using incorrect or outdated library names (e.g., "react-query" vs "tanstack/query")</description>
<instead>Try alternative names or broader search terms if library not found</instead>
</avoid>
</anti_patterns>

<rules priority="critical">
<rule>Always resolve library ID before fetching documentation</rule>
<rule>Verify trust score is 7 or higher before using library documentation</rule>
</rules>

<rules priority="standard">
<rule>Use specific topics to reduce token usage</rule>
<rule>Check snippet count for documentation quality indicators</rule>
<rule>Use versioned IDs when available for specific versions</rule>
<rule>Combine with Serena for codebase verification workflows</rule>
</rules>
