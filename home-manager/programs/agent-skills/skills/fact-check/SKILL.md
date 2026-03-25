---
name: fact-check
description: "Use when the agent needs to 'verify claims', 'fact check', 'validate documentation', 'check sources', or verify external references against authoritative sources. Provides systematic fact verification using Context7 MCP and WebSearch with confidence scoring."
---

Systematic fact-checking of claims against authoritative external sources using Context7, WebSearch, and WebFetch.

## Verification Workflow

1. **Extract claims** that reference external sources (APIs, docs, standards)
2. **Select verification source** based on claim type
3. **Query source** and collect evidence
4. **Assess confidence** on a 0-100 scale
5. **Report discrepancies** for claims with confidence below 80

## Tools

- **resolve-library-id**: Resolve package name to Context7-compatible library ID (call before get-library-docs)
- **get-library-docs**: Fetch library documentation to verify claims (`context7CompatibleLibraryID`, `topic`, `tokens`)
- **WebSearch**: Search web for verification of general claims (`query`)
- **WebFetch**: Fetch specific URL content for verification (`url`, `prompt`)

## Claim Types to Verify

| Claim Type | Example | Primary Source |
|-----------|---------|---------------|
| Library API | "useState returns a tuple" | Context7 |
| Documentation reference | "according to the React docs" | Context7 / WebFetch |
| Standard compliance | "follows WCAG 2.1 AA" | WebSearch |
| Version-specific behavior | "in React 18, Suspense..." | Context7 with version topic |
| Performance claims | "O(log n) complexity per MDN" | WebSearch / WebFetch |

### Example: Verifying a Version-Specific Claim

```
Claim: "React 18 introduces automatic batching for all updates"
Step 1: resolve-library-id libraryName="react"
Step 2: get-library-docs context7CompatibleLibraryID="/facebook/react" topic="batching React 18"
Result: Confirmed - React 18 automatically batches state updates inside promises, setTimeout, and native event handlers
Confidence: 95
```

## Source Selection

1. **Context7** for library documentation (prefer trust score 7+)
2. **WebFetch** for specific URLs cited in claims
3. **WebSearch** for general technical claims and standards
4. Mark as **unverifiable** if no authoritative source is available

### Authoritative Sources

- **Libraries**: Context7 MCP (React: `/facebook/react`, Next.js: `/vercel/next.js`, TypeScript: `/microsoft/typescript`, NixOS: `/nixos/nixpkgs`)
- **Web standards**: MDN (`developer.mozilla.org`), W3C (`w3.org`), WHATWG (`html.spec.whatwg.org`)
- **Security**: OWASP (`owasp.org`)

## Confidence Assessment

| Score | Meaning | Action |
|-------|---------|--------|
| 90-100 | Exact match with authoritative source | Mark verified |
| 80-89 | Strong match, minor wording differences | Mark verified |
| 70-79 | Partial match, some details unverified | Flag for review |
| 60-69 | Weak match, significant uncertainty | Flag for review |
| 0-59 | No match or contradictory evidence | Mark disputed |

**Threshold**: Flag all claims with confidence below 80.

## Discrepancy Report Format

```
Claim: [Original assertion]
Source: [Where claim was made]
Verification source: [Context7/WebSearch result]
Evidence: [Actual information from source]
Confidence: [0-100]
Recommendation: [Suggested correction or note]
```

## Anti-Patterns

- **Assumption verification**: Never mark claims verified without querying a source
- **Single source reliance**: Cross-reference with multiple sources when confidence is 70-85
- **Ignoring version context**: Always verify against the appropriate documentation version
- **Over-verification**: Focus on claims referencing external sources, not obvious facts

## Critical Rules

- Always verify claims against authoritative sources before flagging
- Use Context7 as primary source for library and framework claims
- Document evidence source for every verification result
- Cross-reference disputed claims with multiple sources

## Error Escalation

| Severity | Example | Action |
|----------|---------|--------|
| Low | Claim cannot be verified (missing docs) | Note as unverifiable |
| Medium | Conflicting information from sources | Cross-reference, document both |
| High | Claim contradicts authoritative source | Flag with evidence |
| Critical | Security-related claim is incorrect | Escalate immediately |

## Related Skills

- `investigation-patterns`: Evidence collection methodology
- `technical-documentation`: Documentation accuracy standards
