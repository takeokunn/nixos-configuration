---
name: technical-documentation
description: "Use when asked to write documentation, create a README, produce API docs, draft a design document, write a specification, or create a user guide. Provides structured documentation patterns for developers, teams, and end-users in English and Japanese."
---

Structured patterns for creating technical documentation including READMEs, design documents, API specifications, and user guides. The agent should match documentation structure to the audience's knowledge level and verify all code examples before including them.

## Critical Rules

- Verify all code examples compile and run before including in documentation
- Match documentation structure to audience knowledge level
- Never publish documentation with placeholder content or TODOs
- Use active voice and present tense for instructions

## Workflow

1. **Plan** — Determine document type, audience, and language; create outline
2. **Draft** — Write content with working examples; follow progressive disclosure
3. **Review** — Verify technical accuracy against implementation; test code examples
4. **Maintain** — Update documentation when code changes

## Document Type Selection

| Type | Audience | When to Use |
|------|----------|-------------|
| README | Developers, contributors | Main project entry point, quick start |
| API Specification | Developers integrating with API | REST APIs, GraphQL schemas, SDK interfaces |
| Design Document | Team members, reviewers | Major features, architectural changes, refactors |
| User Guide | Non-technical users, admins | Help documentation, tutorials, product guides |

## README Structure

```markdown
# Project Name

[![Badge](https://img.shields.io/badge/example-badge-blue)]

Brief one-line description of what the project does.

## Features
- Feature 1
- Feature 2
- Feature 3

## Quick Start
\```bash
npm install package-name
\```

## Basic Usage
\```typescript
import { example } from "package-name";
const result = example();
console.log(result);
\```

## Documentation
See [full documentation](link) for detailed guides.

## Contributing
Contributions welcome! See [CONTRIBUTING.md](link).

## License
MIT
```

## API Documentation Structure

The agent should document each endpoint with: method, path, parameters, response format, and error codes.

```markdown
## Authentication
All requests require an API key:
Authorization: Bearer YOUR_API_KEY

## GET /v1/users
Retrieve a list of users.

**Parameters:**
- `limit` (integer, optional): Number of results (default: 10)
- `offset` (integer, optional): Pagination offset (default: 0)

**Response (200):**
```json
{ "users": [{ "id": 1, "name": "John Doe" }], "total": 100 }
```

**Errors:** 401 Unauthorized, 429 Rate limit exceeded
```

## Design Document Structure

1. **Summary** — Problem, solution, scope (one paragraph each)
2. **Background and motivation** — Context for the change
3. **Goals and non-goals** — Explicit scope boundaries
4. **Technical design** — Architecture, data flow, components
5. **Alternatives considered** — Pros/cons of each, decision rationale
6. **Security/privacy considerations** — Encryption, auth, input validation
7. **Testing strategy** — Unit, integration, E2E coverage plan
8. **Rollout plan** — Phased deployment approach

## User Guide Structure

1. **Getting started** — Welcome and prerequisites
2. **Core concepts** — Key terms and mental models
3. **Step-by-step tutorials** — Numbered instructions with expected outcomes
4. **Feature reference** — Comprehensive feature documentation
5. **Troubleshooting / FAQ** — Common issues with solutions
6. **Glossary** — Term definitions

## Best Practices

### Audience-First Approach

- **Developers:** Assume technical background, focus on implementation details
- **Team members:** Balance context with technical depth
- **End users:** Avoid jargon, use step-by-step instructions

### Progressive Disclosure

1. Quick start for immediate value
2. Common use cases
3. Advanced configuration
4. Edge cases and troubleshooting

### Content Quality

- Use descriptive headings and bullet points for scannability
- Include working, copy-pasteable code examples with expected output
- Use tables for structured data
- Bold key terms sparingly
- Test all code examples before publishing

## Language Guidelines

### English

- Active voice, present tense: "Run this command to start the server."
- Professional but approachable tone
- Avoid unnecessarily complex words and idioms that do not translate

### Japanese

- User docs: です・ます調 (polite form); technical specs: である調
- 丁寧だが簡潔 — polite but concise
- Avoid: 過度なカタカナ語、曖昧な表現

### Bilingual

- Maintain parallel structure between languages
- Keep code examples identical; translate only prose
- Use consistent terminology (create glossary if needed)

## Anti-Patterns to Avoid

- **Wall of text** — break into paragraphs, bullet points, headings, and code blocks
- **Outdated info** — document current state, not history
- **Assuming context** — define terms on first use, link to prerequisites
- **Untested examples** — always verify code compiles and runs
- **Passive voice** — "Run the command" not "The command should be run"
- **Vague instructions** — avoid "simply" or "just"; provide specific numbered steps
- **Missing prerequisites** — list required knowledge and setup at the beginning

## Review Checklist

The agent should verify before publishing:

1. Technical accuracy verified against implementation
2. Code examples tested and producing expected output
3. Links working and pointing to correct targets
4. Content appropriate for target audience
5. Grammar and spelling checked

## Error Escalation

- **Low:** Minor formatting inconsistency — fix formatting, follow style guide
- **Medium:** Outdated information detected — update content, verify with code
- **High:** Incorrect technical information — stop, verify with implementation before publishing
- **Critical:** Security-sensitive information exposed — block publication, require security review
