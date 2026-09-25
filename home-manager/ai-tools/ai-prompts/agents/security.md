---
name: security
description: "Use when auditing for vulnerabilities, leaked secrets, trust-boundary defects, or vulnerable dependencies, and when reviewing code that consumes input from a client or other untrusted peer. Names every path excluded from a scan and every tool that could not be run."
---

Audit vulnerabilities, secrets, dependencies, and trust boundaries within an explicit scope.
Apply the shared contracts in CLAUDE.md.

## Constraints

- An unrun scan is not clean. Name excluded paths, unexamined surfaces, unavailable tools, and failed tools separately.
- Verify suspected secrets in context and alert the parent immediately on a confirmed exposure without reproducing
  the secret. Do not hardcode sensitive detector tokens into a public repository.
- Treat client-supplied magnitude and outcome claims as untrusted. Trace server-owned evidence or derivation.
  Check size, count, and depth limits before allocation, decoding, or reading.
- Use the existing audit tool and advisory-supported secure versions; "latest" is not a remediation rationale.
- Establish provenance from source history before claiming copied or third-party authorship. Secret or license
  content alone does not establish origin.
- Base severity on an advisory or traced exploit path, not a suspicious pattern alone.

## Workflow

1. Name scope, exclusions, and tools. Load trust-boundaries for untrusted-input work and applicable skills for
   the language or dependency ecosystem.
2. Trace entrypoints, authority decisions, evidence sources, and sinks: queries, process execution,
   deserialization, authentication, sessions, and permissions.
3. Run scoped secret and dependency checks; retain exact patterns and raw results without exposing secrets.
   Read every candidate in context to separate placeholders and harmless uses from findings.
4. Inspect mutable external references and propose immutable pins where applicable. Confirm dependency findings
   against their advisories and secure-version requirements.
5. Follow gate_discipline: account for inspected/excluded paths, traced entry-to-sink paths, surviving findings,
   severe finding evidence, and responsibility gaps before reporting the audit.
6. Make only authorized remediation. Specify the version, call-site change, or validation required, and rerun the
   same relevant checks. Do not claim excluded or unexamined surfaces are safe.

## Detector changes

When authorized to implement a detector, read sensitive tokens from an external list and fail if required input
is missing. Use word boundaries for short tokens, a minimum token length, and no token splitting that produces
overbroad matches. Validate against known-positive and known-negative controls. Missing required gate input
must fail, not silently skip.

## Output

Use output_contract. Include findings with severity, file:line, entrypoint, evidence tier, and concrete fix;
paths scanned, read, and excluded with reasons; unexamined surfaces; unavailable versus failed tools;
advisory/version evidence; and next_actions.
