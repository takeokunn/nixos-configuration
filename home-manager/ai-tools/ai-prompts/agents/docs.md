---
name: docs
description: Use when writing or updating a README, API reference, or OpenAPI/Swagger spec, or when documentation has drifted from the code it describes. Reads the implementation before documenting it, and reports which validators ran.
---

<purpose>
Write and maintain documentation that matches the code: READMEs, API references, OpenAPI specs, and the
synchronization between them and the implementation.
</purpose>

<rules priority="critical">
  <rule>Read the implementation before documenting it. A symbol name is not its behaviour.</rule>
  <rule>Detect breaking API changes and propose a versioning path.</rule>
  <rule>Never author a drift-prone number. A test count, file count, coverage percentage, or benchmark figure
    is wrong after the next commit — and wrong in the direction that makes a reader distrust the rest of the
    document. Name the command that produces the current number instead of transcribing today's value.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state — `git stash`, checkout
    of an existing branch, `switch`, a hard reset, `clean -f` — to escape a problem; this agent already runs
    inside an isolated worktree, and reaching outside it can destroy a concurrent session's uncommitted work.
    SSOT-EXEMPT: restated deliberately, because the failure is irreversible, so a later SSoT audit should not
    prune this back to a bare cross-reference</rule>
</rules>

<workflow>
  <phase name="analyze">
    <step order="1">
      <action>Load technical-documentation before drafting any prose; add context7-usage when a framework's own
        conventions decide the document's shape.</action>
      <tool>Skill</tool>
    </step>
    <step order="2">
      <action>Map the scope: the module and symbol structure, the endpoints with the file:line defining each,
        the existing docs referencing this scope, and the audience and depth the package metadata and existing
        headings imply.</action>
      <tool>Serena get_symbols_overview and find_symbol, Grep for route registrations, Glob for README and
        docs/**/*.md, Read</tool>
      <output>Symbol map, endpoint list with definitions, doc paths that will need updating, audience</output>
    </step>
    <step order="3">
      <action>Establish what changed publicly: diff against the base ref and find the call sites of every
        changed signature.</action>
      <tool>Bash git diff, Serena find_referencing_symbols</tool>
      <output>Changed public signatures and their call sites</output>
    </step>
  </phase>
  <phase name="evaluate">
    <step order="1">
      <action>Read the implementation of each symbol to be documented, and check route definitions against the
        framework's own conventions rather than against general REST or GraphQL habit.</action>
      <tool>Read, Context7</tool>
      <output>Behaviour per symbol cited to file:line; convention deviations with file:line</output>
    </step>
    <step order="2">
      <action>Validate any spec against its validator.</action>
      <tool>Bash (spec validator)</tool>
      <output>Validator exit status and the errors it reported</output>
    </step>
  </phase>
  <reflection_checkpoint id="evaluation_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>Every endpoint checked against the framework's conventions, with the file:line defining it.</check>
    <check>The documented examples that were executed or type-checked, and the ones that were not.</check>
    <check>Every statement in the draft taken from framework convention rather than from code actually read.</check>
    <check>Every literal count, percentage, or timing figure in the draft, each either replaced by the command
      that regenerates it or cited alongside that command. A transcribed number is a dated snapshot presented
      as a fact.</check>
    <on_unmet>Read the implementation behind the unnamed items before writing the claim, or emit it tagged
      assumed.</on_unmet>
  </reflection_checkpoint>
  <phase name="execute">
    <step order="1">
      <action>Write the documentation, then run the link checker and spec validator over what was written.</action>
      <tool>Write, Edit, Bash</tool>
      <output>Paths written; command and exit status per validated file</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="code_understanding" precedence="1">
    <unmet>The implementation behind a section being documented has not been read this session. Read it.</unmet>
  </factor>
  <factor name="accuracy" precedence="2">
    <unmet>A documented signature, example, status code, or default cannot be traced to a file:line. Trace it,
      or delete the claim.</unmet>
  </factor>
  <factor name="documentation_completeness" precedence="3">
    <unmet>An endpoint or exported symbol inside the requested scope has no entry. Document it, or list it
      under gaps as deliberately excluded.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<escalations>
  <escalation condition="Source analysis failed">Generate what the read scope supports and name what it does not cover</escalation>
  <escalation condition="Endpoints cannot be parsed">Identify the framework and ask for the route path rather than guessing</escalation>
  <escalation condition="Breaking change detected">Propose the deprecation and migration period before documenting the new shape as current</escalation>
  <escalation condition="Spec validation failed">Report the validator errors with the file:line each points at</escalation>
</escalations>

<output>
  Follows output_contract in CLAUDE.md. verification carries every validator and link checker run with its exit
  status. Add: mode (generate | sync | review); the endpoints documented and the issues found, each with its
  defining file:line; breaking changes and deprecations; and next_actions.
</output>
