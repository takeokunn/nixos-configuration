---
name: context7-usage
description: Context7 MCP documentation retrieval patterns for up-to-date library and API references.
version: 1.0.0
---

<purpose>
Provide consistent usage patterns for Context7 MCP so prompts can retrieve current documentation and avoid stale API assumptions.
</purpose>

<tools>
  <tool name="context7_resolve-library-id">Resolve library identifiers before documentation retrieval</tool>
  <tool name="context7_get-library-docs">Fetch focused documentation for specific topics and versions</tool>
</tools>

<patterns>
  <pattern name="resolve_then_fetch">
    <description>Always resolve library id first, then fetch docs with a focused topic.</description>
    <example>
      Resolve library id for the target package, then fetch docs for routing/hooks/config relevant to the task.
    </example>
  </pattern>
  <pattern name="version_sensitive_queries">
    <description>When behavior differs by version, request docs for the exact version or clearly specify baseline assumptions.</description>
    <example>
      Use a versioned library id when available and compare migration notes before updating prompts.
    </example>
  </pattern>
</patterns>

<decision_tree name="context7_usage">
  <question>Do you need current library/framework API behavior?</question>
  <branch condition="Yes">Use Context7 resolve and fetch flow before making claims</branch>
  <branch condition="No">Use local codebase evidence first</branch>
</decision_tree>

<related_agents>
  <agent name="explore">Use for local codebase context before external doc lookup</agent>
  <agent name="general-purpose">Use for synthesis when multiple doc sources conflict</agent>
</related_agents>

<related_skills>
  <skill name="fact-check">Use with external verification workflows</skill>
  <skill name="serena-usage">Use with project-local evidence gathering</skill>
</related_skills>

<rules priority="critical">
  <rule>Keep guidance evidence-based and version-aware</rule>
</rules>
<rules priority="standard">
  <rule>Prefer project conventions over generic defaults</rule>
</rules>

<best_practices>
  <practice priority="high">Apply this skill only when domain fit is clear</practice>
</best_practices>

<error_escalation>
  <level severity="low"><example>Minor inconsistency</example><action>Document and continue</action></level>
  <level severity="medium"><example>Ambiguous guidance</example><action>Clarify assumptions</action></level>
  <level severity="high"><example>Conflicting constraints</example><action>Escalate with options</action></level>
  <level severity="critical"><example>Unsafe or misleading retrieval guidance</example><action>Block and escalate to maintainers with corrective steps</action></level>
</error_escalation>

<constraints>
  <must>Prefer official or primary documentation sources</must>
  <must>State uncertainty when docs are unavailable or ambiguous</must>
  <avoid>Relying on stale memory of API signatures without retrieval</avoid>
</constraints>
