---
name: Technical Documentation
description: This skill should be used when the user asks to "write documentation", "create README", "API docs", "design document", "specification", "user guide", or needs documentation guidance. Provides comprehensive documentation patterns for developers, teams, and end-users in both English and Japanese.
version: 0.1.0
---

<purpose>
Provide structured patterns for creating technical documentation including README, design documents, API specifications, and user guides for diverse audiences.
</purpose>

<document_types>
<type name="readme">
<description>Project introduction and quick start guide</description>
<audience>Developers, contributors, users</audience>
<structure>

<section>Project title and badges</section>
<section>One-line description</section>
<section>Key features (3-5 bullet points)</section>
<section>Quick start / Installation</section>
<section>Basic usage example</section>
<section>Documentation links</section>
<section>Contributing / License</section>
</structure>
</type>

<type name="api_specification">
<description>API reference documentation</description>
<audience>Developers integrating with the API</audience>
<structure>
<section>Overview and authentication</section>
<section>Base URL and versioning</section>
<section>Endpoints (method, path, parameters, response)</section>
<section>Error codes and handling</section>
<section>Rate limits</section>
<section>Examples (curl, language-specific)</section>
</structure>
</type>

<type name="design_document">
<description>Technical design and architecture documentation</description>
<audience>Team members, reviewers, future maintainers</audience>
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
</type>

<type name="user_guide">
<description>End-user facing documentation</description>
<audience>Non-technical users, administrators</audience>
<structure>
<section>Getting started</section>
<section>Core concepts</section>
<section>Step-by-step tutorials</section>
<section>Feature reference</section>
<section>Troubleshooting / FAQ</section>
<section>Glossary</section>
</structure>
</type>
</document_types>

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

# Good

Run this command to start the server.

# Bad

The server can be started by running the following command.
</example>
</english>

<japanese>
<style>です・ます調 (polite form) for user docs, である調 for technical specs</style>
<tone>丁寧だが簡潔</tone>
<avoid>過度なカタカナ語、曖昧な表現</avoid>
<example>
# Good
以下のコマンドでサーバーを起動します。

# Bad

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

<pattern name="readme_structure">
<description>Standard structure for README documentation</description>
<example>
# Project Name

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
# API Reference

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
# Feature Name Design Document

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
# User Guide

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
