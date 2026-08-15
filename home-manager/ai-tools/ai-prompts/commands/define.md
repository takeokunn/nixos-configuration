---
argument-hint: [message]
description: Requirements definition command
---

<purpose>
Produce the specification the user approves before work starts: clarify the problem, the constraints, and the
decisions that actually need deciding. Read-only.
</purpose>

<scope>
  <when_to_use>
    <case>Unclear scope with several design choices — "add authentication", "refactor the data layer"</case>
    <case>A change spanning 3+ files or 2+ system layers</case>
    <case>Irreversible or high-risk work — schema migration, breaking API change, auth rework</case>
    <case>A request whose framing may not reflect the real need</case>
  </when_to_use>
  <when_not_to_use>
    <case>A bug with a clear isolated cause — /bug</case>
    <case>A one-line change or an already-specified task — /execute</case>
    <case>Defined requirements needing only technical investigation — /ask</case>
    <case>Documentation-only changes — /markdown</case>
  </when_not_to_use>
</scope>

<rules priority="critical">
  <rule>Never modify or create a file and never write code. Implementing during this command removes the
    approval step it exists to create. Serena write_memory is not a file write and is permitted.</rule>
  <rule>Say plainly when a request is technically impossible or rests on a capability that is not there. A
    specification that assumes it becomes wasted implementation, discovered late.</rule>
  <rule>Challenge the framing before accepting it. The stated problem is usually the user's first solution, and
    specifying it forecloses the better one — requirements describe the problem; solutions come in /execute.
    Rephrase the request as subject → object → operation to expose hidden ambiguity; if the rephrasing feels
    different from the original, there is a clarification gap.</rule>
  <rule>Never score the document — feasibility, objectivity, confidence, completeness — on a numeric scale. A
    score has no derivation, cannot be checked or disputed, and reads as a measurement. State the observable
    condition: which capability was found at which file:line, which was not found and where it was searched
    for. Express effort in quantities that were counted — files touched, call sites returned by
    find_referencing_symbols, layers crossed, tests affected — never in clock hours, which depend on who does
    the work and what interrupts them.</rule>
</rules>
<rules priority="standard">
  <rule>Signal → hypothesis → verify → conclude. Never jump from signal straight to a question: one that
    investigation could have answered spends the user's turn. There is no question budget for what
    investigation cannot settle, though — an ambiguity resolved now costs a sentence rather than a rewrite.</rule>
  <rule>Reason Why → How → What, and assess system impact (L0) before implementation detail (L4). The levels:
    L0 systems and cross-cutting concerns, L1 data and schema, L2 interfaces and contracts, L3 business rules
    and flow, L4 files and configuration. Reasoning that starts at What specifies the solution already in
    hand.</rule>
  <rule>Specify only what is load-bearing. Detail spent on what any competent implementer would choose anyway
    crowds out the decisions that need deciding, and an exhaustive document that glosses the hard parts is
    worse than a short one — length is not quality. Specify behavior and constraints, never function names,
    variable names, or algorithms.</rule>
  <rule>Start from the minimum scope that satisfies the core need and expand only on demonstrated necessity,
    not on "might be needed later". Three similar future cases are required before generalizing. Ask whether
    the need survives fewer components, which parts are nice-to-have, and whether a phased split delivers value
    in Phase 1.</rule>
  <rule>Account for the full blast radius. Specifying one component while ignoring what it affects produces a
    document that is wrong at implementation time.</rule>
  <rule>Verify a capability exists at the current ref before designing around it. Recall about past states goes
    stale.</rule>
  <rule>Mark one option (Recommended) whenever AskUserQuestion presents choices, so the user reviews a proposal
    rather than doing the analysis.</rule>
  <rule>Internal investigation stays internal. The document is synthesis, never a paste of agent output.</rule>
</rules>

<request_signals>
  Read the request for these before forming any question.

  <signal pattern="A solution is described — add X, change Y to Z, use library A">
    The real requirement may be hidden behind it; the user has already narrowed. Ask what problem it solves,
    whether simpler solutions exist, and whether it fits the existing architecture.</signal>
  <signal pattern="A behavior is described — make it faster, show errors, support format X">
    Acceptance criteria may be clear while scope and approach are open. Find which component owns the behavior,
    the measurable threshold, and the constraints.</signal>
  <signal pattern="A regression is referenced — it broke, this stopped working, used to work">
    Cause and symptom may differ and the fix scope may exceed the reported location. Establish when it broke,
    what changed, and where else the same cause lives.</signal>
  <signal pattern="Vague scope words — everywhere, all, the whole">
    "All" almost never means all; it means the places the user is aware of. Enumerate the actual locations by
    searching, not by trusting the description.</signal>
  <signal pattern="A capability not yet in the codebase is implied">
    A hidden dependency on a library, service, or infrastructure. Establish whether it exists, the cost of
    introducing it, and whether existing primitives suffice.</signal>
  <signal pattern="just or simple — just add a field, simple change">
    The user may be unaware of the blast radius. Map dependents, migrations, API consumers, and test
    coverage.</signal>
</request_signals>

<workflow>
  <phase name="load">
    <step order="1">
      <action>Load define-core and requirements-definition before anything else. define-core holds the phase
        sequence this command executes — prepare, analyze, investigate, clarify, verify, document, finalize —
        and requirements-definition holds the methodology inside those phases: question scoring, FR format,
        acceptance-criteria shape. Neither is in context until loaded, so skipping this leaves the command with
        no workflow at all. Load fact-check as well when the requirements depend on external behavior that must
        be confirmed rather than recalled.</action>
      <tool>Skill</tool>
      <output>Skills loaded, and the phase list define-core returned so the rest can be checked against it</output>
    </step>
  </phase>
  <phase name="run_core_workflow">
    <step order="1">
      <action>Run define-core's phases in its order, applying what this file adds at each: the Why → How → What
        ordering and L0-before-L4 depth, the request_signals table, the minimum-scope rule, and the output
        contract below. Where define-core and this file both speak to a decision, this file governs — it is the
        narrower context. Dispatch investigation by need: explore for existing patterns and reference
        implementations, design for architectural consistency and alternatives, database for schema and
        migration implications, general-purpose for completeness and dependency risk, validator for
        contradictions between specifications. Send the independent ones in one message.</action>
      <output>Phases run, agents dispatched and skipped, and any phase skipped with its reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Any workflow phase skipped, and why.</check>
  <check>That no file was created or modified, or name the file that was.</check>
  <on_unmet>Resolve the gap before delivering the document.</on_unmet>
</reflection_checkpoint>

<decision_criteria>
  <factor name="requirement_clarity" precedence="1">
    <unmet>A requirement admits two readings that would produce different implementations. Ask with
      AskUserQuestion; do not write the reading that is cheaper to specify.</unmet>
  </factor>
  <factor name="technical_feasibility" precedence="2">
    <unmet>The document assumes a capability — a library, an API, a schema column — not located in this
      codebase or confirmed via Context7. Verify it, or record it as an outstanding issue.</unmet>
  </factor>
  <factor name="stakeholder_alignment" precedence="3">
    <unmet>A design decision the user has not answered is being written as settled. Put it back to the user, or
      move it to outstanding issues so the finalize gate sees it.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md, delivering a requirements document with these sections. Use tables and
  Mermaid where structure is easier seen than read, and order abstract before concrete.

  <section name="Summary" required="always">The request in one sentence, the Why behind it, expected outcomes</section>
  <section name="Current State" required="always">Existing system, stack, affected components</section>
  <section name="Functional Requirements" required="always">FR-001 format, marked mandatory or optional, at behavior level</section>
  <section name="Non-Functional Requirements" required="when-applicable">Performance, security, maintainability</section>
  <section name="Technical Specifications" required="always">Design policy, impact scope, each key decision with its rationale and the alternatives rejected</section>
  <section name="Architecture Impact" required="when-multi-layer">Diagram when 2+ layers are affected; dependency changes</section>
  <section name="Data / Schema Changes" required="when-applicable">ERD or schema diff</section>
  <section name="Interface / API Changes" required="when-applicable">Endpoint table or contract diff</section>
  <section name="Constraints" required="always">Technical and operational</section>
  <section name="Test Requirements" required="always">Unit, integration, and acceptance criteria stated as observable behavior rather than internal mechanism</section>
  <section name="Outstanding Issues" required="always">Unresolved questions and anything asked for that this document does not specify, with reasons; "none" is stated explicitly. A disagreement with the user goes here too: when the investigation reaches a different severity or priority than the user assigned, record both assessments and what each rests on, and hand the decision back. Silently deferring buries the risk; silently escalating overrides a call that was the user's to make.</section>
  <section name="Task Breakdown" required="always">Phased tasks with files, overview, and dependencies, plus the decisions, references, and constraints /execute needs — including what it must NOT assume</section>
</output>

<completion_conditions>
  <done_when>
    <criterion>Every critical question is answered or recorded as outstanding</criterion>
    <criterion>Every requirement is grounded in codebase evidence, and feasibility rests on a located capability
      rather than an assumed one</criterion>
    <criterion>Scope is bounded, the blast radius stated, and a simpler scope satisfying the core need has been
      considered and rejected for a reason</criterion>
    <criterion>Acceptance criteria are observable behavior</criterion>
    <criterion>The document specifies decision points rather than the obvious</criterion>
    <criterion>The handoff carries enough context that a fresh implementer proceeds without re-asking</criterion>
    <criterion>When outstanding issues are non-empty, define-core's finalize gate ran and the user chose to
      resolve, defer, or stop — rather than the command silently ending. Correctly skipped when "none"</criterion>
  </done_when>
</completion_conditions>
