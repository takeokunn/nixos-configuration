---
name: Technical Documentation
description: This skill should be used when the user asks to "write documentation", "create README", "API docs", "design document", "specification", "user guide", or needs documentation guidance. Provides comprehensive documentation patterns for developers, teams, and end-users in both English and Japanese.
---

<purpose>
Provide structured patterns for creating technical documentation including README, design documents, API specifications, and user guides for diverse audiences.
</purpose>

<tools>
<tool>Write - Create new documentation files</tool>
<tool>Edit - Update existing documentation</tool>
<tool>Read - Review existing documentation and code</tool>
<tool>Grep - Search for patterns across documentation</tool>
<tool>Glob - Find related documentation files</tool>
</tools>

<concepts>
<concept name="document_types">Four primary types: README (project intro), API spec (endpoints/interfaces), design doc (architecture decisions), user guide (end-user tutorials)</concept>
<concept name="audience_levels">Developer (technical depth), team member (context + depth), end user (no jargon, step-by-step)</concept>
<concept name="progressive_disclosure">Start with quick start, then common cases, then advanced config, finally edge cases</concept>
<concept name="documentation_lifecycle">Plan (outline) → Draft (write + examples) → Review (verify accuracy) → Maintain (update with code)</concept>
</concepts>

<patterns>
<pattern name="readme_document_type">
<description>Project introduction and quick start guide</description>
<decision_tree name="when_to_use">
<question>Is this the main entry point for project documentation?</question>
<if_yes>Create README with quick start and overview</if_yes>
<if_no>Consider using design doc for detailed architecture or user guide for end-user documentation</if_no>
</decision_tree>
<context>
<audience>Developers, contributors, users</audience>
<when_to_use>Creating or updating main project documentation</when_to_use>
</context>
<structure>
<section>Project title and badges</section>
<section>One-line description</section>
<section>Key features (3-5 bullet points)</section>
<section>Quick start / Installation</section>
<section>Basic usage example</section>
<section>Documentation links</section>
<section>Contributing / License</section>
</structure>
</pattern>

<pattern name="api_specification_document_type">
<description>API reference documentation</description>
<decision_tree name="when_to_use">
<question>Are you documenting API endpoints or SDK interfaces?</question>
<if_yes>Create API specification with authentication, endpoints, and examples</if_yes>
<if_no>Use README for library usage or design doc for internal architecture</if_no>
</decision_tree>
<context>
<audience>Developers integrating with the API</audience>
<when_to_use>Documenting REST APIs, GraphQL schemas, or SDK interfaces</when_to_use>
</context>
<structure>
<section>Overview and authentication</section>
<section>Base URL and versioning</section>
<section>Endpoints (method, path, parameters, response)</section>
<section>Error codes and handling</section>
<section>Rate limits</section>
<section>Examples (curl, language-specific)</section>
</structure>
</pattern>

<pattern name="design_document_type">
<description>Technical design and architecture documentation</description>
<decision_tree name="when_to_use">
<question>Are you proposing a major feature or architectural change?</question>
<if_yes>Create design document with technical details, alternatives, and rollout plan</if_yes>
<if_no>Use inline code comments for small changes or README for usage instructions</if_no>
</decision_tree>
<context>
<audience>Team members, reviewers, future maintainers</audience>
<when_to_use>Proposing new features, architectural changes, or major refactors</when_to_use>
</context>
<structure>
<section>Summary (problem, solution, scope)</section>
<section>Background and motivation</section>
<section>Goals and non-goals</section>
<section>Technical design (architecture, data flow)</section>
<section>Alternatives considered</section>
<section>Security / Privacy considerations</section>
<section>Testing strategy</section>
<section>Rollout plan</section>
</structure>
</pattern>

<pattern name="user_guide_document_type">
<description>End-user facing documentation</description>
<decision_tree name="when_to_use">
<question>Is your audience non-technical end users?</question>
<if_yes>Create user guide with step-by-step tutorials and troubleshooting</if_yes>
<if_no>Use API docs for developers or README for contributors</if_no>
</decision_tree>
<context>
<audience>Non-technical users, administrators</audience>
<when_to_use>Creating help documentation, tutorials, or product guides</when_to_use>
</context>
<structure>
<section>Getting started</section>
<section>Core concepts</section>
<section>Step-by-step tutorials</section>
<section>Feature reference</section>
<section>Troubleshooting / FAQ</section>
<section>Glossary</section>
</structure>
</pattern>

<pattern name="readme_structure">
<description>Standard structure for README documentation</description>
<example>
<note>Project Name</note>

[![Badge](https://img.shields.io/badge/example-badge-blue)]

Brief one-line description of what the project does.

<features>

- Feature 1
- Feature 2
- Feature 3

<quick start>

```bash
npm install package-name
```

<basic usage>

```typescript
import { example } from "package-name";

const result = example();
console.log(result);
```

<documentation>

See [full documentation](link) for detailed guides.

<contributing>

Contributions welcome! See [CONTRIBUTING.md](link).

<license>

MIT
</example>
</pattern>

<pattern name="api_documentation">
<description>Comprehensive API reference documentation structure</description>
<example>
<note>API Reference</note>

<authentication>

All requests require an API key in the Authorization header:

```bash
Authorization: Bearer YOUR_API_KEY
```

<base url>

```
https://api.example.com/v1
```

<endpoints>

<get_users>

Retrieve a list of users.

**Parameters:**

- `limit` (integer, optional): Number of results (default: 10)
- `offset` (integer, optional): Pagination offset (default: 0)

**Response:**

```json
{
  "users": [
    { "id": 1, "name": "John Doe" },
    { "id": 2, "name": "Jane Smith" }
  ],
  "total": 100
}
```

**Error Codes:**

- `401`: Unauthorized - Invalid API key
- `429`: Rate limit exceeded
  </example>
  </pattern>

<pattern name="design_document_structure">
<description>Technical design document format for architectural decisions</description>
<example>
<note>Feature Name Design Document</note>

<summary>

**Problem:** Brief description of the problem being solved
**Solution:** High-level approach
**Scope:** What's included and what's not

<background>

Context and motivation for this design.

<goals and non-goals>

**Goals:**

- Goal 1
- Goal 2

**Non-Goals:**

- What we're explicitly not doing
- Future considerations

<technical design>

<architecture>

[Diagram or description of system architecture]

<data flow>

1. User action
2. System processing
3. Response

<components>

**Component A:** Responsible for X
**Component B:** Responsible for Y

<alternatives considered>

<alternative 1>

Pros: ...
Cons: ...
Decision: Not chosen because...

<security considerations>

- Data encryption at rest and in transit
- Authentication and authorization
- Input validation

<testing strategy>

- Unit tests for component logic
- Integration tests for API contracts
- E2E tests for critical user flows

<rollout plan>

1. Phase 1: Internal testing
2. Phase 2: Beta release (10% of users)
3. Phase 3: Full rollout
   </example>
   </pattern>

<pattern name="user_guide_structure">
<description>End-user documentation with step-by-step instructions</description>
<example>
<note>User Guide</note>

<getting started>

Welcome! This guide will help you get started with [Product Name].

<core concepts>

**Workspace:** A container for your projects
**Project:** A collection of related items
**Item:** The basic unit of work

<creating your first project>

1. Click the "New Project" button
2. Enter a project name
3. Choose a template (optional)
4. Click "Create"

You'll see your new project in the sidebar.

<troubleshooting>

<troubleshooting_login>

1. Check your email address is correct
2. Click "Forgot Password" to reset
3. Contact support if the issue persists

<troubleshooting_data>

Ensure you have a stable internet connection. The app auto-saves every 30 seconds.

<glossary>

**Term:** Definition
**Another Term:** Another definition
</example>
</pattern>
</patterns>

<best_practices>
<practice priority="critical">
<description>Audience-first approach - Write for your specific audience's knowledge level</description>
<example>
Developers: Assume technical background, focus on implementation details
Team members: Balance context with technical depth
End users: Avoid jargon, use step-by-step instructions
</example>
</practice>

<practice priority="critical">
<description>Progressive disclosure - Start with essentials, reveal complexity gradually</description>
<example>
1. Quick start for immediate value
2. Common use cases
3. Advanced configuration
4. Edge cases and troubleshooting
</example>
</practice>

<practice priority="high">
<description>Make content scannable to enable quick information retrieval</description>
<example>
- Use descriptive headings
- Use bullet points for lists
- Include code blocks with syntax highlighting
- Use tables for structured data
- Use bold for key terms (sparingly)
</example>
</practice>

<practice priority="high">
<description>Example-driven documentation - Show, don't just tell</description>
<example>
- Include working code examples
- Show expected output
- Provide copy-pasteable commands
</example>
</practice>

<practice priority="medium">
<description>Active voice and present tense for clarity</description>
<example>
Good: Run this command to start the server.
Bad: The server can be started by running the following command.
</example>
</practice>

<practice priority="medium">
<description>Test all code examples before publishing</description>
<example>
Always verify that code examples compile and run correctly
Include expected output
Test edge cases mentioned in documentation
</example>
</practice>
</best_practices>

<language_guidelines>
<english>

<style>Active voice, present tense</style>

<tone>Professional but approachable</tone>
<avoid>Unnecessarily complex words, idioms that don't translate</avoid>
<example>

<good_example>Good</good_example>

Run this command to start the server.

<bad_example>Bad</bad_example>

The server can be started by running the following command.
</example>
</english>

<japanese>
<style>です・ます調 (polite form) for user docs, である調 for technical specs</style>
<tone>丁寧だが簡潔</tone>
<avoid>過度なカタカナ語、曖昧な表現</avoid>
<example>
<good_example>Good</good_example>
以下のコマンドでサーバーを起動します。

<bad_example>Bad</bad_example>

サーバーの起動については、下記コマンドを実行することで可能となります。
</example>
</japanese>

<bilingual>
<rule>Maintain parallel structure between languages</rule>
<rule>Keep code examples identical, translate only prose</rule>
<rule>Use consistent terminology (create glossary if needed)</rule>
</bilingual>
</language_guidelines>

<workflow>
<phase name="plan">
<step>Identify document type and audience</step>
<step>Outline key sections</step>
<step>Gather technical details</step>
</phase>

<phase name="draft">
<step>Write in order: overview → details → examples</step>
<step>Include all code examples (test them)</step>
<step>Mark TODOs for uncertain parts</step>
</phase>

<phase name="review">
<step>Verify technical accuracy</step>
<step>Check all links and code examples work</step>
<step>Review for audience appropriateness</step>
<step>Proofread for grammar and typos</step>
</phase>

<phase name="maintain">
<step>Update when code changes</step>
<step>Review periodically for accuracy</step>
<step>Track user feedback</step>
</phase>
</workflow>

<output>
<format>
<document plan>

- Type: [readme/api_spec/design_doc/user_guide]
- Audience: [developer/team/end_user]
- Language: [en/ja/both]

<structure>

[Proposed sections based on document type]

<content>

[Actual documentation content]

<review checklist>

- [ ] Technical accuracy verified
- [ ] Code examples tested
- [ ] Links working
- [ ] Appropriate for audience
- [ ] Grammar and spelling checked
      </format>
      </output>

<anti_patterns>
<avoid name="wall_of_text">
<description>Long paragraphs without formatting</description>
<instead>Break into smaller paragraphs, use bullet points, headings, and code blocks</instead>
</avoid>

<avoid name="outdated_info">
<description>Documenting historical context instead of current state</description>
<instead>Document what exists now, move history to a separate section if needed</instead>
</avoid>

<avoid name="assuming_context">
<description>Using terms or concepts without definition</description>
<instead>Define terms on first use, link to prerequisites, provide glossary</instead>
</avoid>

<avoid name="untested_examples">
<description>Including code examples that haven't been tested</description>
<instead>Always verify code examples compile and run correctly before publishing</instead>
</avoid>

<avoid name="passive_voice">
<description>Using passive constructions that obscure agency</description>
<instead>Use active voice for clarity (e.g., "Run the command" not "The command should be run")</instead>
</avoid>

<avoid name="jargon_overload">
<description>Using technical jargon without explanation</description>
<instead>Define technical terms on first use, provide a glossary, or use simpler language for user-facing docs</instead>
</avoid>

<avoid name="missing_prerequisites">
<description>Assuming users have required knowledge or setup</description>
<instead>List prerequisites clearly at the beginning, link to setup guides</instead>
</avoid>

<avoid name="vague_instructions">
<description>Using imprecise language like "simply" or "just" without concrete steps</description>
<instead>Provide specific, numbered steps with expected outcomes</instead>
</avoid>
</anti_patterns>

<rules priority="critical">
<rule>Verify all code examples compile and run before including in documentation</rule>
<rule>Match documentation structure to audience knowledge level</rule>
<rule>Never publish documentation with placeholder content or TODOs</rule>
</rules>

<rules priority="standard">
<rule>Use active voice and present tense for instructions</rule>
<rule>Include expected output for all code examples</rule>
<rule>Define technical terms on first use or link to glossary</rule>
<rule>Keep README under 500 lines; link to detailed docs for more</rule>
</rules>

<error_escalation>
<level severity="low">
<example>Minor formatting inconsistency</example>
<action>Fix formatting, follow style guide</action>
</level>
<level severity="medium">
<example>Outdated information detected</example>
<action>Update content, verify with code</action>
</level>
<level severity="high">
<example>Incorrect technical information</example>
<action>Stop, verify with implementation before publishing</action>
</level>
<level severity="critical">
<example>Security-sensitive information exposed</example>
<action>Block publication, require security review</action>
</level>
</error_escalation>

<constraints>
<must>Verify accuracy against actual implementation</must>
<must>Include runnable code examples</must>
<must>Follow project documentation style</must>
<avoid>Documenting without reading code</avoid>
<avoid>Adding timestamps to documents</avoid>
<avoid>Duplicating information unnecessarily</avoid>
</constraints>

<related_agents>
<agent name="design">Requirements analysis and documentation structure planning</agent>
<agent name="docs">Technical documentation writing and generation</agent>
<agent name="execute">Documentation deployment and publishing tasks</agent>
<agent name="bug">Fixing broken links, outdated examples, and documentation inconsistencies</agent>
</related_agents>

<related_skills>
<skill name="serena-usage">Symbol operations for extracting code examples and API signatures</skill>
<skill name="context7-usage">Library documentation lookup for accurate API references</skill>
<skill name="investigation-patterns">Analyzing codebases to understand features for documentation</skill>
<skill name="technical-writing">Creating blog posts and tutorials from documentation</skill>
</related_skills>
