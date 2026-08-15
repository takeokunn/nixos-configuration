---
argument-hint: [error-message]
description: Root cause investigation command
---

<purpose>
Trace an error or anomalous behavior to its cause from evidence. Produces an explanation the user decides what
to do with; applies no fix.
</purpose>

<rules priority="critical">
  <rule>Never modify a file and never implement a fix. Applying a change forecloses the user's decision while
    the cause is still a hypothesis. Serena write_memory is not a file write and is permitted.</rule>
  <rule>Judge from the logs and the code, not from the user's account. The reported location is where the
    symptom surfaced, which is frequently not where the defect lives.</rule>
  <rule>Report honestly when the cause cannot be identified. A named-but-unevidenced cause is worse than an
    open question, because it ends the search and the next reader inherits it as settled.</rule>
</rules>
<rules priority="standard">
  <rule>Logs are the primary source: they record what actually happened, where code records only what can
    happen.</rule>
  <rule>Track the occurrence path chronologically, and find every location sharing the defect's shape before
    recommending anything.</rule>
</rules>

<investigation_hazards>
  <hazard name="call_site_role">A call site tells you a code path exists, not what role it plays. Debug hooks
    and QA controls are simpler and more findable than the production implementation, so "the only calls I can
    find are manual" is a common route to declaring a feature unimplemented when it is merely owned elsewhere —
    and the resulting recommendation is to build a second one.</hazard>
  <hazard name="generated_document_as_source">Where a committed document and its generator both exist, the
    generator is the evidence and the document is a claim. A checked-in schema snapshot or generated client
    answers the question in the exact form it was asked and goes stale silently.</hazard>
  <hazard name="changed_error_is_not_progress">An attempted fix that turns one error into a different one has
    usually cleared a surface obstacle in front of the real constraint. Reading that as progress is what
    produces the loop of trying successive flags and tokens. The test is whether the condition identified as
    the cause is gone, not whether the output differs.</hazard>
</investigation_hazards>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load investigation-patterns; it governs hypothesis discharge, bisection, and evidence handling —
        the substance of this command. Load fact-check as well when the failure implicates an external library
        or API contract.</action>
      <tool>Skill</tool>
      <output>Skills loaded, and why fact-check was skipped if it was</output>
    </step>
    <step order="2">
      <action>Activate the Serena project and call list_memories. Read the entries matching this failure's
        domain — {domain}-patterns, architecture-*, {project}-conventions — and none if none match.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Memories read, or "nothing in the index matched"</output>
    </step>
  </phase>

  <phase name="analyze">
    <step order="1">
      <observe>The artifact the symptom was actually observed against — the loaded module, binary, or daemon,
        with its path, mtime, or hash — against the build output in the working tree. And how many failures
        arrived together, against how many things changed.</observe>
      <reason>The investigation's subject is not established until these two are. Whenever the runtime loads
        from an install prefix, a long-lived daemon, a container image, a compiled bundle, or a package cache
        rather than the working tree, the code about to be read may not be the code that ran — and every later
        step reads consistently to a confident wrong answer, because the line that was removed is still in the
        artifact. The failure-count ratio is the harness-versus-code question in its cheapest form.</reason>
      <act>Name the artifact and state whether it matches current source. If it does not, stop and report that
        instead of investigating, because nothing read afterwards describes what ran. State whether the subject
        is the code or the harness, and what decided it.</act>
    </step>
    <step order="2">
      <action>Classify the error — syntax, runtime, logic, config — from the message, exception type, and any
        stack trace. Record the primary location as file:line together with the call chain, and mark the
        distinction between the symptom site and the likely cause site: the deepest non-library frame is where
        it surfaced, not necessarily where it originated.</action>
      <output>Classification, symptom site, call chain</output>
    </step>
    <step order="3">
      <action>Read the log lines surrounding and preceding the error, and the events immediately before, during,
        and after it. This is what separates a new failure from a recurring pattern, and a transient condition
        — a race, resource exhaustion — from a deterministic defect.</action>
      <output>Timeline with state anomalies; the failure classified deterministic or condition-dependent</output>
    </step>
  </phase>

  <phase name="investigate">
    <step order="1">
      <action>Scale the dispatch to the failure. A stack trace pointing at one file you can read is not an
        agent's work. Where the failure spans subsystems, dispatch in one message: quality-assurance for the
        failure mechanism and ranked hypotheses, explore for the error site and every recurrence, and
        general-purpose for the log timeline and dependency state. Name what you skipped and why.</action>
      <tool>Task</tool>
      <output>Findings with file:line, or the reason no agent was needed</output>
    </step>
    <step order="2">
      <action>Read the failing code in full, then its dependency and import chain, then the config values in
        effect and the recent changes touching them. Verify any external contract against Context7 or the
        vendored source rather than recall.</action>
      <tool>Read, Grep, Serena find_symbol and find_referencing_symbols, Bash (git log, git diff), Context7</tool>
      <output>The failing line identified, its chain, and the config and changes bearing on it</output>
    </step>
    <step order="3">
      <action>Whenever a reproduction, probe, or narrowed slice fails, decide before recording it whether the
        failure is telling you about the subject or about the reproduction. A slice can cut through an
        incomplete form, a wrapper can resolve a relative path against its own location, a probe can reference
        a symbol never defined in the reduced file. The useful tell: a failure arriving immediately, before any
        of the suspect work could have started, is nearly always the reproduction's own. Name the observation
        that rules out the other explanation.</action>
      <output>Each failure labelled subject-side or reproduction-side, with what settled it</output>
    </step>
    <iteration_limit>3</iteration_limit>
    <iteration_rationale>Narrowing assumes a stable oracle. If the boundary moves between probes — a different
      file, form, or line each time — the oracle is noisy and every further step fits that noise; and a
      narrowing loop never runs out of plausible next moves, so it will not stop on its own. After three
      narrowing steps without a boundary that reproduces, stop: re-run one identical probe to test
      reproducibility, then report what has been ruled out and hand the scope decision to the user.</iteration_rationale>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The evidence chain as symptom → mechanism → cause, citing a file:line or log line at each link.</check>
    <check>The link that was inferred rather than read, or that every link was read.</check>
    <check>The other locations sharing this cause, or that the search ran and found none.</check>
    <on_unmet>Continue on the unsupported link. If it cannot be established from the repository, say so rather
      than presenting the chain as complete.</on_unmet>
  </reflection_checkpoint>

  <phase name="self_evaluate">
    <step order="1">
      <action>Tag each link per the evidence rules in CLAUDE.md, downgrading any that cannot name the command
        run or the file:line read. Where a link rests on a document rather than code, scope the tier to the
        passage checked — a document can be accurate in one section and describe classes, columns, or features
        that exist nowhere in another. Then list what the reported error raises that this report does not
        answer, and set the status.</action>
      <output>Tagged chain, downgrades, gaps, status</output>
    </step>
  </phase>

  <phase name="persist">
    <step order="1">
      <action>An investigation produces a finding rather than a change, so nothing forces it to be acted on and
        the next investigation reads the same code to the same conclusion without meeting the earlier note.
        Call list_memories and search it for a prior recording of this finding; if one exists, say in the
        report that this is a repeat and cite it — that the same conclusion has been reached more than once is
        the argument for acting on it, and it is invisible from any single run. Write only against the
        memory_policy triggers in CLAUDE.md; otherwise output "persist: no triggers matched — skip".</action>
      <tool>Serena list_memories, write_memory or edit_memory</tool>
      <output>Memory written or edited with whether this is a repeat, or the explicit skip</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Per gate_discipline in CLAUDE.md.</gate>
  <check>Any workflow phase skipped, and why.</check>
  <check>That no file was modified and no fix applied, or name what was changed.</check>
  <on_unmet>Resolve the gap before returning the report.</on_unmet>
</reflection_checkpoint>

<agents>
  Read-only, dispatched by need. Each finding carries a file:line.

  <agent name="quality-assurance" subagent_type="quality-assurance">The failure mechanism, and hypotheses
    ranked with the evidence separating them.</agent>
  <agent name="explore" subagent_type="explore">The error site with its surrounding code and call chain, plus
    every location sharing the same defect shape.</agent>
  <agent name="general-purpose" subagent_type="general-purpose">Log timeline, environment anomalies, dependency
    state.</agent>
</agents>

<decision_criteria>
  <factor name="root_cause_certainty" precedence="1">
    <unmet>The named cause was never observed producing the symptom — no reproduction, no log line, no code
      path read end to end. Present it as a ranked hypothesis, not as the root cause.</unmet>
  </factor>
  <factor name="evidence_chain" precedence="2">
    <unmet>A link has no file:line or log line behind it. Read it, or mark it inferred and say what would close
      it.</unmet>
  </factor>
  <factor name="fix_viability" precedence="3">
    <unmet>The suggested fix was not checked against every location sharing the cause. Run the recurrence
      search, or state that the recommendation covers only the reported site.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. Add:

  <section name="root_cause">Direct cause, underlying cause, and the conditions under which it fires, with the
    chain tagged link by link.</section>
  <section name="impact">Scope, and the other locations sharing this cause.</section>
  <section name="recommendations">Fix suggestions without implementation, and prevention.</section>
  <section name="fix_scope_bracket">The smallest change that could resolve the cause and the largest the
    evidence would justify, named as two ends, with everything between conditioned on what would have to be
    shown first — "include the storage change only if evidence shows cross-world reads are involved". State the
    failure at each end: what an under-fix leaves wrong, what an over-fix puts at risk. The reader's next
    decision is how far to go, not what the cause is, and this investigation holds the evidence for it.</section>
  <section name="subject">Whether the investigation treated the code or the harness as its subject, the
    artifact the symptom was observed against, and whether it matched current source.</section>
</output>
