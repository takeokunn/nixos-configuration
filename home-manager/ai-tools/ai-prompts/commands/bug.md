---
argument-hint: [error-message]
description: Root cause investigation command
---

<purpose>
Identify root causes from error messages and anomalous behavior, providing fact-based analysis without performing fixes.
</purpose>
<rules priority="critical">
  <rule>Never modify a file and never implement a fix. This command produces an explanation the user
    decides what to do with; applying a change forecloses that decision while the cause is still a
    hypothesis.</rule>
  <rule>Judge from the logs and the code, not from the user's account of the failure. The reported
    location is where the symptom surfaced, which is frequently not where the defect lives.</rule>
  <rule>Report honestly when the cause cannot be identified. A named-but-unevidenced cause is worse
    than an open question, because it ends the search.</rule>
</rules>
<rules priority="standard">
  <rule>Treat logs as the primary source: they record what actually happened, where code records only
    what can happen.</rule>
  <rule>Track the occurrence path chronologically, and check whether the same defect shape appears
    elsewhere before recommending anything.</rule>
</rules>
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reproducing bugs manually in isolation — AI can trace all call paths and state transitions from a stack trace in a single pass</practice>
    <practice>Treating the error location as the root cause — AI can distinguish symptom from root cause by mapping the full dependency chain</practice>
    <practice>One hypothesis at a time — AI can score multiple hypotheses in parallel and rule out lower-probability causes simultaneously</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Build a complete evidence chain (symptom → mechanism → root cause) before concluding — never jump from signal to verdict</principle>
    <principle>Verify hypotheses against code evidence, not user description alone — the reported location is often not the true source</principle>
    <principle>Map all recurrence locations in a single investigation pass — don't fix only the reported instance</principle>
    <principle>A call site found by search tells you a code path exists, not what role it plays. Debug
      hooks and QA controls are simpler and more findable than the production implementation, so
      "the only calls I can find are manual" is a common route to declaring a feature unimplemented
      when it is merely owned elsewhere — and the resulting recommendation is to build a second one.</principle>
    <principle>When a committed document and its generator both exist in the repository, the generator
      is the evidence and the document is a claim. A checked-in schema snapshot or generated client
      answers the question in the exact form it was asked and goes stale silently.</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Load the investigation-patterns skill with the Skill tool. It governs hypothesis
        discharge, bisection, and evidence handling — the substance of this command — and it is not in
        context until loaded; a skill named in a reference attribute never loads itself. If the failure
        implicates an external library or API contract, load fact-check as well.</action>
      <tool>Skill</tool>
      <output>The skills loaded, named; and if fact-check was skipped, the reason</output>
    </step>
    <step order="2">
      <action>Initialize Serena, then classify this task as "investigation" and load only the memories
        matching that type's priority categories — {domain}-patterns, architecture-*,
        {project}-conventions — following the serena-usage skill for the filter procedure</action>
      <tool>Serena activate_project, list_memories, read_memory</tool>
      <output>The memories read, named; or an explicit "nothing in the index matched investigation"</output>
    </step>
  </phase>
  <phase name="analyze">
    <step order="1">
      <observe>The artifact the symptom was actually observed against — the loaded module, binary, or
        daemon and its path, mtime, or hash — against the build output in the working tree. Also how
        many failures arrived together, against how many things changed.</observe>
      <reason>The investigation's subject is not established until these two are. Whenever the runtime
        loads from an install prefix, a long-lived daemon, a container image, a compiled bundle, or a
        package cache rather than the working tree, the code about to be read may not be the code that
        ran, and every later step will read consistently to a confident wrong answer because the line
        that was removed is still in the artifact. The failure-count ratio is the harness-versus-code
        question in its cheapest form.</reason>
      <act>Name the artifact and state whether it matches current source; if it does not, stop and
        report that instead of investigating, because nothing read afterwards describes what ran. State
        whether the subject of this investigation is the code or the harness, and what decided it.</act>
    </step>
    <step order="2">
      <observe>Error message text, exception type, and any provided stack trace or logs</observe>
      <reason>Error classification (syntax / runtime / logic / config) determines the investigation branch and which agents are most relevant</reason>
      <act>Classify error type; record classification as input to investigate phase delegation</act>
    </step>
    <step order="3">
      <observe>Stack trace line numbers, file names, and call chain depth</observe>
      <reason>The deepest non-library frame is the symptom location — not necessarily the root cause; the full chain reveals how control flow reached the failure point</reason>
      <act>Record primary error location (file:line) and call chain; flag the distinction between symptom site and likely root cause site</act>
    </step>
    <step order="4">
      <observe>Available log output: timestamps, log levels, preceding events</observe>
      <reason>Logs provide temporal context to distinguish a new failure from a recurring pattern, and reveal system state at failure time</reason>
      <act>Identify log lines directly preceding and surrounding the error; note any state anomalies</act>
    </step>
    <step order="5">
      <observe>Events immediately before, during, and after the error occurrence</observe>
      <reason>Temporal context distinguishes transient conditions (race, resource exhaustion) from deterministic bugs (logic error, missing null check)</reason>
      <act>Record the error trigger sequence; classify as deterministic or condition-dependent</act>
    </step>

  </phase>
  <phase name="investigate">
    <step order="1">
      <action>Delegate to quality-assurance agent: analyze stack trace, error patterns</action>
      <tool>Task tool (quality-assurance)</tool>
      <output>Error classification and ranked hypotheses</output>
    </step>
    <step order="2">
      <action>Delegate to explore agent: find error location and related code paths</action>
      <tool>Task tool (explore)</tool>
      <output>Error site with file:line, call chain, recurrence locations</output>
    </step>
    <step order="3">
      <action>Delegate to general-purpose agent: analyze logs and dependencies</action>
      <tool>Task tool (general-purpose)</tool>
      <output>Log timeline, dependency issues</output>
    </step>
    <step order="4">
      <action>Use fact-check skill patterns: verify external documentation references via Context7</action>
      <tool>Context7 MCP, WebSearch</tool>
      <output>Verified external claims, flagged claims</output>
    </step>
    <step order="5">
      <action>Analyze error location details from agent findings</action>
      <tool>Read, Serena find_symbol</tool>
      <output>The failing code read in full, with the line that fails identified</output>
    </step>
    <step order="6">
      <action>Review dependencies and imports</action>
      <tool>Read, Grep, Serena find_referencing_symbols</tool>
      <output>Dependency and import chain of the failing module</output>
    </step>
    <step order="7">
      <action>Check config files and recent changes</action>
      <tool>Read, Grep, Bash (git log, git diff — read-only inspection)</tool>
      <output>Config values in effect and the recent changes touching them</output>
    </step>
    <step order="8">
      <action>Whenever a reproduction, probe, or narrowed slice is built and it fails, decide before
        recording the failure whether it is telling you about the subject or about the reproduction.
        A slice can cut through an incomplete form, a wrapper can resolve a relative path against its
        own location, a probe can reference a symbol that was never defined in the reduced file. The
        useful tell: a failure arriving immediately, before any of the suspect work could have
        started, is nearly always the reproduction's own. Name the observation that rules out the
        other explanation.</action>
      <output>Each recorded failure labelled subject-side or reproduction-side, with what settled it</output>
    </step>
    <iteration_limit>3</iteration_limit>
    <iteration_rationale>Narrowing assumes a stable oracle. If the boundary moves between probes —
      a different file, form, or line each time — the oracle is noisy and every further step is
      fitting that noise, and a narrowing loop never runs out of plausible next moves, so it will not
      stop on its own. After three narrowing steps without a boundary that reproduces, stop narrowing:
      re-run one identical probe to test reproducibility, then report what has been ruled out and hand
      the scope decision to the user. This mirrors the single-iteration limit /execute-full already
      imposes, for the same reason.</iteration_rationale>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>State the evidence chain as symptom → mechanism → cause, citing a file:line or log line at each link.</check>
    <check>Name the link in that chain that was inferred rather than read, or state that every link was read.</check>
    <check>Name the other locations sharing this root cause, or state that the search was run and found none.</check>
    <on_unmet>Continue investigating the unsupported link. If it cannot be established from the repository,
      say so in the report rather than presenting the chain as complete.</on_unmet>
  </reflection_checkpoint>
  <phase name="gather">
    <step order="1">
      <action>Collect runtime info (OS, versions, env vars)</action>
      <tool>Bash (read-only inspection commands)</tool>
      <output>Runtime environment, with the command that produced each value</output>
    </step>
    <step order="2">
      <action>Check resources (disk, memory, network)</action>
      <tool>Bash (read-only inspection commands)</tool>
      <output>Resource state, or "not checked" with the reason</output>
    </step>
  </phase>
  <phase name="self_evaluate">
    <step order="1">
      <action>Re-read the report and tag each finding. A finding tagged verified must name the command
        run or the file:line read; if it cannot, downgrade it. Where a link rests on a document rather
        than on code, scope the tier to the passage checked — a document can be accurate in one section
        and describe classes, columns, or features that exist nowhere in another.</action>
      <output>Findings tagged, over-claims downgraded, document-sourced tiers scoped to their passage</output>
    </step>
    <step order="2">
      <action>List anything the reported error asked about that this report does not answer, and why —
        not attempted, blocked by missing logs, or judged out of scope</action>
      <output>Gap list, possibly empty</output>
    </step>
    <step order="3">
      <action>Set the status from what steps 1 and 2 found, then append the self_feedback section
        naming the weakest claim and what would confirm it</action>
      <output>Status and self_feedback</output>
    </step>
  </phase>
  <phase name="persist">
    <objective>Capture reusable debugging insights to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: did this investigation reveal a reusable debugging pattern,
        an architectural insight, or a recurring bug class?
        Call list_memories to check if a memory for this topic already exists, and search it for a prior
        recording of this same finding. An investigation produces a finding rather than a change, so
        nothing forces the finding to be acted on, and the next investigation reads the same code to the
        same conclusion without ever meeting the earlier note. If a prior recording exists, say in the
        report that this is a repeat and cite it: that the same conclusion has now been reached more
        than once is the argument for acting on it, and it is not visible from any single run.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no, and whether this finding is a repeat</output>
    </step>
    <step order="2">
      <action>If trigger matched: use edit_memory (existing topic) or write_memory (new topic).
        Note: write_memory is Serena memory only — this does not violate the read-only file constraint.
        For write_memory: prepend memory_content_format frontmatter (serena-usage skill)
        with domain, status=active, created=YYYY-MM, last-verified=YYYY-MM.
        For edit_memory on a memory lacking frontmatter: add it, updating last-verified.
        If no trigger matched: output "persist: no triggers matched — skip"</action>
      <tool>Serena edit_memory or write_memory</tool>
      <output>Memory entry updated with frontmatter (name listed), or explicit skip reason</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
  <check>Name any workflow phase that was skipped, and why.</check>
  <check>State that no file was modified and no fix was applied — this command investigates only — or name what was changed.</check>
  <on_unmet>Resolve the structural gap before returning the report.</on_unmet>
</reflection_checkpoint>
<agents>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">
    <role>Analyze error patterns, stack traces, and code defects to identify failure mechanisms</role>
    <receives>error_message, stack_trace, file_paths[], reproduction_steps</receives>
    <produces>error_classification{type, mechanism}, defects[]{location: file:line, description}, hypotheses[]{cause, evidence_tier, evidence}</produces>
    <done_when>Evidence chain from symptom to root cause established with a citation at each link, or competing hypotheses ranked with the evidence separating them</done_when>
  </agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">
    <role>Analyze logs, runtime environment, and dependency relationships for contextual evidence</role>
    <receives>log_content, environment_info{os, versions, env_vars}, dependency_list[]</receives>
    <produces>log_analysis{critical_events[], timeline}, env_anomalies[], dependency_issues[]</produces>
    <done_when>All available log entries processed; environmental factors assessed with file:line references where applicable</done_when>
  </agent>
  <agent name="explore" subagent_type="explore" readonly="true">
    <role>Locate error sites, trace call paths, and find all recurrence locations for the same root cause</role>
    <receives>error_location{file, line}, symbol_names[], search_patterns[]</receives>
    <produces>error_site{surrounding_code, call_chain}, related_paths[], recurrences[]{file:line, similarity_reason}</produces>
    <done_when>Error site mapped; all locations sharing the same root cause pattern identified</done_when>
  </agent>
</agents>
<execution_graph>
  <parallel_group id="error_analysis" depends_on="none">
    <agent>quality-assurance</agent>
    <agent>explore</agent>
  </parallel_group>
  <parallel_group id="context_gathering" depends_on="none">
    <agent>general-purpose</agent>
  </parallel_group>
  <sequential_step id="synthesis" depends_on="error_analysis,context_gathering">
    <agent>quality-assurance</agent>
    <reason>Requires findings from both error analysis and context gathering</reason>
  </sequential_step>
</execution_graph>
<delegation>
  <requirement>Full error message/stack trace</requirement>
  <requirement>Reproduction steps (if known)</requirement>
  <requirement>Related file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
</delegation>
<decision_criteria>
  <factor name="root_cause_certainty" precedence="1">
    <unmet>The named cause was never observed producing the symptom — no reproduction, no log line, no
      code path read end to end. Present it as a ranked hypothesis, not as the root cause. A changed
      error message is not evidence either: an attempted fix that turns one error into a different one
      has usually cleared a surface obstacle in front of the real constraint, and reading the change as
      progress is what produces the loop of trying successive flags and tokens. The test is whether the
      condition originally identified as the cause is gone, not whether the output differs.</unmet>
  </factor>
  <factor name="evidence_chain" precedence="2">
    <unmet>A link between symptom and cause has no file:line or log line behind it. Read that link, or
      mark it inferred and say what would close it.</unmet>
  </factor>
  <factor name="fix_viability" precedence="3">
    <unmet>The suggested fix has not been checked against every location sharing the root cause. Run the
      recurrence search before recommending it, or state that the recommendation covers only the reported site.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <overview>Summary of error and investigation</overview>
    <log_analysis>Critical log information, error context</log_analysis>
    <code_analysis>Relevant code, identified issues</code_analysis>
    <root_cause>
- Direct cause
- Underlying cause
- Conditions</root_cause>
    <evidence_tiers>Each link of the chain tagged, with the file:line, log line, or command that backs it</evidence_tiers>
    <impact>Scope, similar errors</impact>
    <recommendations>Fix suggestions (no implementation), prevention</recommendations>
    <fix_scope_bracket>The smallest change that could resolve the cause, and the largest change the
      evidence would justify — named as two ends, with everything between them conditioned on what
      would have to be shown first ("include the storage change only if evidence shows cross-world
      reads are involved"). State the failure at each end: what an under-fix would leave wrong, and
      what an over-fix would put at risk. The reader's next decision is how far to go, not what the
      cause is, and this investigation holds the evidence that decision needs.</fix_scope_bracket>
    <further_investigation>Unclear points, next steps</further_investigation>
    <self_feedback>
      <subject>Whether the investigation treated the code or the harness as its subject, the artifact
        the symptom was observed against, and whether that artifact matched current source</subject>
      <downgrades>Any link first written as verified that could not name its evidence, and the tier it was moved to</downgrades>
      <weakest_claim>The link in the chain resting on the thinnest evidence, and what would confirm it</weakest_claim>
      <gaps>Anything the reported error raises that this report does not answer, and why</gaps>
    </self_feedback>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="BUG-B001" priority="critical">
      <trigger>Before reading any source in the analyze phase</trigger>
      <action>Name the artifact the symptom was observed against and establish whether it was built
        from the source about to be read</action>
      <verification>The artifact and its provenance appear in the report, or an explicit statement that
        the runtime loads directly from the working tree</verification>
    </behavior>
    <behavior id="BUG-B002" priority="high">
      <trigger>When proposing a fix</trigger>
      <action>Search for every location sharing the same defect shape, and bracket the fix scope at
        both ends</action>
      <verification>Impact analysis and fix_scope_bracket in output</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="BUG-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Concluding without evidence</action>
      <response>Block the conclusion. A named cause ends the search, so an unevidenced one costs more
        than an open question — the next reader inherits it as settled.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation>
  <examples>
    <example severity="low">Minor log warning without impact</example>
    <example severity="medium">Unclear error context or missing stack trace</example>
    <example severity="high">System crash or data corruption detected</example>
    <example severity="critical">Security breach or critical data loss risk</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="ask">When investigation reveals architectural questions</command>
  <command name="define">When bug fix requires requirements specification</command>
  <command name="execute">When ready to implement fix after investigation</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="investigation-patterns">Core debugging methodology</skill>
  <skill name="serena-usage">Navigate error locations efficiently</skill>
  <skill name="testing-patterns">Understand test failures and coverage gaps</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Prioritize logs as primary information source</must>
  <must>Report honestly if cause cannot be identified</must>
  <must>Name the artifact the symptom was observed against before reading source to explain it</must>
  <must>Bracket the recommended fix scope at both ends rather than naming only the cause</must>
  <avoid>Implementing fixes</avoid>
  <avoid>Accepting user speculation without verification</avoid>
  <avoid>Forcing contrived causes when evidence is insufficient</avoid>
</constraints>
