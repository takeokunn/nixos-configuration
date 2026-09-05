---
argument-hint: [error-message]
description: Root cause investigation command
---

<purpose>
Trace an error or anomalous behavior to its cause from evidence: an explanation the user acts on, not a fix.
</purpose>

<rules priority="critical">
  <rule>Never modify a file or fix anything: a change forecloses the user's decision while the cause is a
    hypothesis. Serena write_memory is permitted: it's not a file write.</rule>
  <rule>Judge from the logs and code, not the user's account: the reported location is where the symptom
    surfaced, often not where the defect lives.</rule>
  <rule>Report honestly when the cause can't be identified: a named-but-unevidenced cause is worse than an open
    question, since it ends the search and the next reader inherits it as settled.</rule>
</rules>
<rules priority="standard">
  <rule>Logs are the primary source: they record what happened, code only what can happen.</rule>
  <rule>Track the occurrence path chronologically, and find every location sharing the defect's shape before
    recommending anything.</rule>
</rules>

<investigation_hazards>
  <hazard name="call_site_role">A call site proves a path exists, not its role: debug hooks and QA controls are
    simpler and more findable than the production implementation, so "the only calls I can find are manual"
    often wrongly reads as unimplemented rather than owned elsewhere, so the fix then builds a second
    one.</hazard>
  <hazard name="generated_document_as_source">Where a committed document and its generator both exist, the
    generator is the evidence, the document only a claim: a checked-in schema snapshot or generated client
    answers in the exact form asked and goes stale silently.</hazard>
  <hazard name="changed_error_is_not_progress">A fix that turns one error into another has usually just cleared
    a surface obstacle in front of the real constraint, so reading that as progress produces the loop of trying
    successive flags and tokens. The test: whether the cause condition is gone, not whether the output
    differs.</hazard>
</investigation_hazards>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load investigation-patterns (it governs hypothesis discharge, bisection, and evidence handling
        here), plus fact-check when the failure implicates an external library or API contract.</action>
      <tool>Skill</tool>
      <output>Skills loaded, and why fact-check was skipped if it was</output>
    </step>
    <step order="2">
      <action>Activate the Serena project, call list_memories, and read entries matching this failure's domain
        ({domain}-patterns, architecture-*, {project}-conventions), or none if none match.</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>Memories read, or "nothing in the index matched"</output>
    </step>
  </phase>

  <phase name="analyze">
    <step order="1">
      <observe>The artifact the symptom ran against (module, binary, or daemon, path/mtime/hash) versus the
        tree's build output; and how many failures arrived together versus how many things changed.</observe>
      <reason>Subject unclear until both are known: if the runtime loads from an install prefix, long-lived
        daemon, container image, compiled bundle, or package cache rather than the working tree, the code read
        may not be the code that ran, and every later step then reads to a confident wrong answer since the
        removed line is still in the artifact. The failure-count ratio is the harness-versus-code question,
        cheapest form.</reason>
      <act>Name the artifact and state whether it matches current source; if not, stop and report that instead
        of investigating: nothing read afterward describes what ran. State whether the subject is code or
        harness, and what decided it.</act>
    </step>
    <step order="2">
      <action>Classify the error (syntax, runtime, logic, config) from the message, exception type, and stack
        trace. Record the primary location as file:line with the call chain, marking symptom site versus cause
        site: the deepest non-library frame is where it surfaced, not where it originated.</action>
      <output>Classification, symptom site, call chain</output>
    </step>
    <step order="3">
      <action>Read the log lines around and before the error, and the events immediately before, during, and
        after it: this separates a new failure from a recurring pattern, and a transient condition (race,
        resource exhaustion) from a deterministic defect.</action>
      <output>Timeline with state anomalies; the failure classified deterministic or
        condition-dependent</output>
    </step>
  </phase>

  <phase name="investigate">
    <step order="1">
      <action>Scale the dispatch to the failure: a stack trace pointing at one readable file isn't agent work.
        Where the failure spans subsystems, dispatch in one message (quality-assurance for the mechanism and
        ranked hypotheses, explore for the error site and every recurrence, general-purpose for the log timeline
        and dependency state), and name what you skipped and why.</action>
      <tool>Agent</tool>
      <output>Findings with file:line, or the reason no agent was needed</output>
    </step>
    <step order="2">
      <action>Read the failing code in full, then its dependency and import chain, then config values in effect
        and recent changes touching them, verifying any external contract against Context7 or the vendored
        source rather than recall.</action>
      <tool>Read, Grep, Serena find_symbol and find_referencing_symbols, Bash (git log, git diff),
        Context7</tool>
      <output>The failing line identified, its chain, and the config and changes bearing on it</output>
    </step>
    <step order="3">
      <action>When a reproduction, probe, or slice fails, decide before recording it whether the failure
        describes the subject or the reproduction: a slice can cut through an incomplete form, a wrapper can
        resolve a relative path against its own location, a probe can reference a symbol never defined in the
        reduced file. Tell: a failure arriving immediately, before suspect work could start, is nearly always
        the reproduction's own. Name the observation ruling it out.</action>
      <output>Each failure labelled subject-side or reproduction-side, with what settled it</output>
    </step>
    <iteration_limit>3</iteration_limit>
    <iteration_rationale>Narrowing assumes a stable oracle: if the boundary moves between probes (different
      file, form, or line each time), the oracle is noisy and every step fits that noise, and a narrowing loop
      never runs out of next moves, so it won't stop on its own. After three steps without a reproducing
      boundary, stop: re-run one identical probe for reproducibility, then report what's ruled out and hand the
      scope decision to the user.</iteration_rationale>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The evidence chain as symptom → mechanism → cause, citing a file:line or log line at each
      link.</check>
    <check>The link that was inferred rather than read, or that every link was read.</check>
    <check>The other locations sharing this cause, or that the search ran and found none.</check>
    <on_unmet>Continue on the unsupported link. If it cannot be established from the repository, say so rather
      than presenting the chain as complete.</on_unmet>
  </reflection_checkpoint>

  <phase name="self_evaluate">
    <action>Tag each link per CLAUDE.md's evidence rules, downgrading any that can't name the command run or
      file:line read. Where a link rests on a document, scope the tier to the passage checked: one section can
      be accurate while another describes classes, columns, or features nowhere else. List what the reported
      error raises that this report doesn't answer, and set the status.</action>
    <output>Tagged chain, downgrades, gaps, status</output>
  </phase>

  <phase name="persist">
    <action>An investigation produces a finding, not a change, so nothing forces action: the next investigation
      reads the same code to the same conclusion, missing the earlier note. Call list_memories and search for a
      prior recording of this finding; if found, say in the report that this is a repeat and cite it: a
      conclusion reached more than once, invisible from any single run, is itself the argument for acting on it.
      Write only against the memory_policy triggers in CLAUDE.md; otherwise output "persist: no triggers matched,
      skip".</action>
    <tool>Serena list_memories, write_memory or edit_memory</tool>
    <output>Memory written or edited with whether this is a repeat, or the explicit skip</output>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <check>Any workflow phase skipped, and why.</check>
  <check>That no file was modified and no fix applied, or name what was changed.</check>
  <on_unmet>Resolve the gap before returning the report.</on_unmet>
</reflection_checkpoint>

<agents>
  Read-only, dispatched by need. Each finding carries a file:line.

  <agent name="quality-assurance" subagent_type="quality-assurance">Failure mechanism and hypotheses ranked by
    the evidence separating them.</agent>
  <agent name="explore" subagent_type="explore">Error site with surrounding code and call chain, plus every
    location sharing the same defect shape.</agent>
  <agent name="general-purpose" subagent_type="general-purpose">Log timeline, environment anomalies, dependency
    state.</agent>
</agents>

<decision_criteria>
  <factor name="root_cause_certainty" precedence="1">
    <unmet>The named cause was never observed producing the symptom: no reproduction, no log line, no code path
      read end to end. Present it as a ranked hypothesis, not the root cause.</unmet>
  </factor>
  <factor name="evidence_chain" precedence="2">
    <unmet>A link has no file:line or log line behind it. Read it, or mark it inferred and say what would close
      it.</unmet>
  </factor>
  <factor name="fix_viability" precedence="3">
    <unmet>The suggested fix was not checked against every location sharing the cause. Run the recurrence
      search, or state that the recommendation covers only the reported site.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md. Add:

  <section name="root_cause">Direct cause, underlying cause, and the conditions it fires under, with the chain
    tagged link by link.</section>
  <section name="impact">Scope, and the other locations sharing this cause.</section>
  <section name="recommendations">Fix suggestions without implementation, and prevention.</section>
  <section name="fix_scope_bracket">The smallest change that could resolve the cause and the largest the
    evidence would justify, as two ends: everything between conditioned on what must be shown first, e.g.
    "include the storage change only if evidence shows cross-world reads are involved." State the failure at
    each end: what an under-fix leaves wrong, an over-fix risks. The reader's next decision is how far to go,
    not what the cause is: this investigation holds that evidence.</section>
  <section name="subject">Whether the investigation treated the code or the harness as its subject, the artifact
    the symptom was observed against, and whether it matched current source.</section>
</output>
