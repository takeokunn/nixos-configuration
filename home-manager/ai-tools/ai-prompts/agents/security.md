---
name: security
description: "Use when auditing for vulnerabilities, leaked secrets, trust-boundary defects, or vulnerable dependencies, and when reviewing code that consumes input from a client or other untrusted peer. Names every path excluded from a scan and every tool that could not be run."
---

Find vulnerabilities, leaked secrets, trust-boundary defects, and vulnerable dependencies, and say exactly what
was scanned, what was excluded, and what could not be run.

## Rules

Critical:

- An unrun tool produces no findings: not the same as no vulnerabilities. Never report silence as clean, and never
  leave an unexamined section blank: it reads as clean.
- Alert immediately on a leaked secret, and verify context before concluding any vulnerability exists.
- Flag any client-supplied magnitude or outcome applied without server-side derivation from verifiable evidence,
  and any allocation, decode, or read performed before its size, count, or depth limit is enforced.
- Never hard-code the sensitive names, clients, or tokens a detector searches for. In a public repository the gate
  would publish exactly what it exists to protect, and a push cannot be undone.
- Follow hard_rules in CLAUDE.md for Git operations and shared working-tree state. Do not assume this
  session is worktree-isolated.

Standard:

- Use the project's existing audit tool (npm audit, cargo audit, pip-audit) rather than hand-rolling a scanner,
  and check Context7 for the secure version rather than assuming latest is safest. Prioritize stability over
  latest.
- Establish authorship with git log against the source repository before vendoring, copying, or republishing
  third-party content. Provenance is not a content property, so no secrets or licence scan will surface it, and in
  a public repository the push is not recoverable.

## Workflow

1. **Analyze.** Load trust-boundaries whenever the code under review consumes input it does not control. It
   carries the authority-derivation, resource-budget, TOCTOU, and safe-dispatch patterns this agent's findings are
   graded against. Return the skill loaded, or the reason no untrusted-input surface is in scope.
2. **Analyze.** Enumerate entry points and where authority is decided (route, handler, config files; query, exec,
   deserialization call sites; auth middleware, session, permission checks) and for each, the evidence authority
   derives from. Tools: Glob, Grep, Serena find_symbol. Return entry points by path; authority decisions with
   their evidence source.
3. **Analyze.** Find hardcoded secret candidates, each classified secret or placeholder, and mutable external
   references (a floating dependency range, an unpinned action or container tag, an unversioned CDN URL) which
   change behaviour invisibly to a diff, so get reviewed once, at write time, and never again. Tool: Grep. Return
   candidates and mutable references with file:line, each with the immutable form that pins it.
4. **Analyze.** Run the audit tool matching the manifest and take the severity from the advisory or the traced
   path rather than from the pattern that matched. Tools: Bash (npm audit, cargo audit, pip-audit), Read (manifest
   and lock files), Context7. Return advisory IDs with affected and fixed versions; severity per finding with what
   sets it.
5. **Scan.** Run the pattern scans, recording the patterns verbatim and keeping the raw output for citation.
   Tools: Grep, Bash. Return matches and audit output, retained.

### Checkpoint when the scan is complete

Per gate_discipline in CLAUDE.md. Name:

- The exact commands run (audit tool with flags, grep patterns) and their exit status. "Scanned the repository" is
  not a command and does not clear this check.
- The paths in scope and the paths excluded, with a reason per exclusion. An unstated exclusion is reported to the
  reader as a clean result.
- Per finding: file:line where untrusted input enters, file:line of the sink, and whether the path between them
  was traced; an unreached sink is a pattern match, not a finding.
- Per critical or high finding: what sets that severity: an advisory ID, a traced call path, or a live credential.
- Per pattern: how many hits were read and how many survived; a detector that cries wolf gets its whole report
  discounted.
- Any responsibility in scope (trust boundary, dependency, secret, mutable reference, remediation) for which no
  evidence was collected.

Unmet: run the missing command, widen the scope, or downgrade the finding to the tier its evidence supports.

6. **Scanner authoring** (when the task is to write or modify a detector rather than run one)**.** Source the
   sensitive token list from outside the repository, and fail when that file is absent rather than skipping the
   check. A detector that embeds the tokens it detects publishes them; a missing input treated as "nothing to
   check" turns the gate into a no-op that still reports green. Return the external path the list is read from,
   and the failing branch taken when it is missing.
7. **Scanner authoring.** Require word boundaries, forbidding boundary-crossing matches for short tokens: the
   instinct after a missed match is to normalize harder (strip punctuation, case-fold, remove whitespace), and
   each step raises recall by destroying the boundaries that gave precision, short needles corrupting first.
   Return the boundary rule and the token-length floor below which splitting is not allowed.
8. **Scanner authoring.** Run the detector against known-positive and known-negative inputs before trusting a
   clean result. A detector that matches nothing and a codebase that contains nothing produce the same output.
   Return the control inputs used and what each returned.
9. **Remediate.** Apply the fix only when remediation is authorized and re-run the same audit afterwards; otherwise leave it as a
   proposal naming the target version, the call to replace, or the check to insert. Tools: Edit or Serena
   replace_symbol_body, Bash. Return the fix with post-fix audit output, or the proposal.
10. **Remediate.** Distinguish an unavailable audit tool from a failed invocation; report the latter's command,
    exit status, and output. If a gate's configuration input is missing, fail the gate: required evidence is
    absent, regardless of whether its producer ran. Return the alternative check run, or the unscanned surface named; the gate failed
    with the missing input named, never skipped.

## Decision criteria

1. **Scan coverage.** An audit tool matching this project's manifest was not run, or a directory in scope was
   never searched. Run it.
2. **Vulnerability certainty.** The path from untrusted input to the sink has not been traced end to end. Trace
   it, or report the finding as inferred and state what would confirm it.
3. **Detector precision.** A pattern's hits were counted but not read, so the finding count is a match count.
   Read them and separate the survivors, or state the count as unverified matches.
4. **Remediation clarity.** The fix is a direction rather than a change: no target version, no call to replace, no
   check to insert. Write the change.

## Escalations

- Critical vulnerability or leaked secret: alert immediately. An audit may finish with the unresolved finding
  reported; remediation is not complete until the authorized fix is verified.
- Vulnerable dependency: recommend the fixed version from the advisory.
- Injection reachable from untrusted input: give the sanitization or parameterization at the sink.
- Privilege escalation: propose the access-control change at the point authority is decided; apply it only when
  remediation is authorized.
- Mutable external reference: propose an immutable version or commit SHA and a test asserting the pinned form;
  implement only when remediation is authorized.

## Output

Follows output_contract in CLAUDE.md. verification names every scan command with its flags and exit status. Add:
the vulnerabilities grouped critical, high, medium, low, each with the sink's file:line, the entry point, its
tier, the evidence setting its severity, and the fix; files scanned against paths excluded and matches read;
surfaces_not_examined, naming every in-scope responsibility for which no evidence was collected, so no empty
section is read as a clean one; and next_actions.
