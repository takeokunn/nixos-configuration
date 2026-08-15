---
name: Investigation Patterns
description: Use when tracing a symptom to its cause — debugging, bisecting, or working out how an unfamiliar implementation behaves. Covers evidence-based analysis, bisecting a symptom whose boundary moves between runs, a probe that measures its own gate rather than the phenomenon, and checking a completion claim against the artifact instead of the summary.
version: 2.4.0
---

<purpose>
  Provide systematic patterns for codebase investigation and debugging, ensuring evidence-based analysis with every finding tagged by how it is known rather than scored.
</purpose>

<tools>
  <tool name="find_symbol">
    <description>Locate specific symbols by name in the codebase</description>
    <param name="name_path_pattern">Pattern to match symbol names</param>
    <param name="relative_path">Optional path to restrict search</param>
    <param name="depth">Depth to retrieve children (default 0)</param>
    <use_case>Finding class, function, or variable definitions</use_case>
  </tool>

  <tool name="get_symbols_overview">
    <description>Get high-level structure of a file</description>
    <param name="relative_path">Path to file to analyze</param>
    <param name="depth">Depth of symbol tree (default 0)</param>
    <use_case>Understanding file organization before detailed investigation</use_case>
  </tool>

  <tool name="find_referencing_symbols">
    <description>Find all references to a symbol</description>
    <param name="name_path">Symbol to find references for</param>
    <param name="relative_path">File containing the symbol</param>
    <use_case>Tracing dependencies and usage patterns</use_case>
  </tool>

  <tool name="Grep">
    <description>Search for regex patterns across codebase (use Bash grep or Glob)</description>
    <param name="substring_pattern">Regular expression to search</param>
    <param name="relative_path">Optional path to restrict search scope</param>
    <use_case>Finding specific patterns or usage across files; follow up with Serena find_symbol or find_referencing_symbols for navigation</use_case>
  </tool>
</tools>

<concepts>
  <concept name="evidence_standards">
    <description>Standards for collecting and reporting evidence, stated as a tier and an observable
      boundary rather than a self-assessed score. A confidence number produced in the same pass that
      did the investigation never contradicts that investigation, so nothing downstream ever reads a
      low score and looks further (CLAUDE.md evidence_and_reporting, core-patterns evidence_tiers).</description>
    <example>
      Citation: Always provide file:line references (path/to/file.ext:line_number)

      Evidence tier — how a finding is known, never a number:

      - verified: A command was run, or the exact lines were read; the finding carries the command and
        its output, or the file:line citation. Anyone can re-run it and get the same answer.
      - inferred: Derived from evidence that was actually read, but the conclusion itself was never
        directly observed. State the evidence and the inferential step, so the step can be disputed.
      - assumed: Taken from convention, prior knowledge, or the user's framing. Nothing in this
        repository was checked. State what would confirm it.

      Coverage — the observable boundary of what was examined, not a percentage:

      - Name the files and symbols actually read.
      - Name what was NOT examined and why (out of scope, not found, deferred).
      - Name the query that established the boundary — the Grep pattern, the find_referencing_symbols
        call, the directory walked — so a reader can re-run it and check whether it was exhaustive.
    </example>
  </concept>

  <concept name="null_reference">
    <description>Null pointer or undefined reference errors</description>
    <example>
      Symptom: NullPointerException, undefined is not a function
      Investigation: Check all paths to the null access
      Fix: Add null checks or ensure initialization
    </example>
  </concept>

  <concept name="race_condition">
    <description>Concurrent access issues</description>
    <example>
      Symptom: Intermittent failures, works sometimes
      Investigation: Look for shared mutable state, async operations
      Fix: Add synchronization or redesign for immutability
    </example>
  </concept>

  <concept name="gated_measurement">
    <description>A counter, sampler, or health check placed inside a conditionally-executed body measures the gate, not the phenomenon</description>
    <example>
      Symptom: A metric reads flat zero (or is stuck far below threshold) while the phenomenon it counts is obviously occurring
      Investigation: Find where the instrumentation lives relative to the guard. If the increment is inside a rate-limited,
        coalesced, debounced, or sampled body, its ceiling is the gate's rate, not the event's rate
      Fix: Move the measurement outside the gate, or count gate-closed events explicitly as a separate signal

      Compounding case: two independent limiters in the same path compose into a dead zone. Each is defensible alone —
      a producer with its own rate limit calling a consumer with its own coalescing gate — and their product makes a
      downstream threshold unreachable. Neither component is wrong; the composition is.

      Why unit tests miss it: tests that call the detection function directly with synthetic counts bypass both gates,
      so they pass while the live path never reaches the threshold at all.
    </example>
  </concept>

  <concept name="heterogeneous_registry">
    <description>A registry that gains a second member variant breaks consumers reading a variant-specific property</description>
    <example>
      Symptom: A consumer that iterates a registry works for most members and produces meaningless or empty results for some
      Investigation: Check whether every member satisfies the property the consumer reads. A docstring or comment that
        narrows the contract ("members must be defined with X") is documentation, not enforcement, and is routinely violated
      Fix: Guard the variant-specific read, and give the discrimination a name

      Duplication as the signal: when the same inline property-presence check appears at several call sites to tell variants
      apart, the missing thing is a named predicate. The duplication is what makes the missing abstraction visible.

      See testing-patterns for tests that enumerate a production registry; the investigation angle here is that a registry
      with mixed member kinds is a likely source of "passes but tests nothing" behavior.
    </example>
  </concept>

  <concept name="off_by_one">
    <description>Boundary condition errors</description>
    <example>
      Symptom: Missing first/last element, index out of bounds
      Investigation: Check loop boundaries and index calculations
      Fix: Verify start/end conditions, use inclusive/exclusive correctly
    </example>
  </concept>

  <concept name="resource_leak">
    <description>Unclosed resources accumulating over time</description>
    <example>
      Symptom: Memory growth, connection exhaustion
      Investigation: Check resource acquisition and release paths
      Fix: Ensure cleanup in finally/defer, use resource management patterns
    </example>
  </concept>

  <concept name="encoding_issue">
    <description>Character encoding mismatches</description>
    <example>
      Symptom: Garbled text, unexpected characters
      Investigation: Trace encoding at each transformation step
      Fix: Ensure consistent encoding throughout pipeline
    </example>
  </concept>

  <concept name="five_whys">
    <description>Ask "why" repeatedly to drill to root cause</description>
    <example>
      Why did the server crash? - Out of memory
      Why out of memory? - Connection pool exhausted
      Why exhausted? - Connections not being released
      Why not released? - Exception bypasses cleanup
      Root cause: Missing try-finally for connection release
    </example>
  </concept>

  <concept name="timeline_analysis">
    <description>Reconstruct sequence of events leading to failure</description>
    <example>
      Collect timestamps from logs
      Order events chronologically
      Identify divergence from expected behavior
    </example>
  </concept>

  <concept name="investigation_output">
    <description>Standard format for investigation results</description>
    <example>
      <question>Restate the question for confirmation</question>
      <investigation>Evidence-based findings with file:line references
        - Source 1: path/to/file.ts:42 - finding description
        - Source 2: path/to/other.ts:15 - finding description</investigation>
      <conclusion>Direct answer based on evidence</conclusion>
      <coverage>Evidence tier per finding (verified/inferred/assumed); what was examined, what was not,
        and the query that established that boundary (evidence_standards)</coverage>
      <recommendations>Suggested actions without implementation</recommendations>
      <unclear_points>Information gaps that would improve the answer</unclear_points>
    </example>
  </concept>

  <concept name="debugging_output">
    <description>Standard format for debugging results</description>
    <example>
      <problem_statement>Clear description of the issue</problem_statement>
      <reproduction_steps>How to reproduce</reproduction_steps>
      <investigation>Evidence collected with file:line references</investigation>
      <root_cause>Identified cause with supporting evidence</root_cause>
      <solution>Proposed fix with rationale</solution>
      <verification>How to verify the fix works</verification>
      <prevention>How to prevent recurrence</prevention>
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="scope_classification">
    <description>Classify the question type to determine investigation approach</description>
    <decision_tree name="when_to_use">
      <question>Does the question require understanding codebase structure or behavior?</question>
      <if_yes>Apply scope classification to determine investigation depth and tools</if_yes>
      <if_no>Consider requirements-definition skill for unclear requirements</if_no>
    </decision_tree>
    <example>
      Architecture: System design, component relationships
      Implementation: Specific code behavior, algorithm details
      Debugging: Error causes, unexpected behavior
      Design: Pattern usage, code organization
    </example>
  </pattern>

  <pattern name="source_identification">
    <description>Identify relevant sources for investigation</description>
    <decision_tree name="when_to_use">
      <question>Is the codebase large or unfamiliar?</question>
      <if_yes>Apply source identification to locate relevant evidence efficiently</if_yes>
      <if_no>Directly examine known sources</if_no>
    </decision_tree>
    <example>
      Code: Use Serena for symbol search and dependency analysis
      Documentation: Check inline comments, README, API docs
      History: Git log for context on changes
      External: Context7 for library documentation
    </example>
  </pattern>

  <pattern name="evidence_collection">
    <description>Collect evidence systematically using appropriate tools</description>
    <decision_tree name="when_to_use">
      <question>Do you have specific symbols or patterns to investigate?</question>
      <if_yes>Apply evidence collection with symbol-level tools</if_yes>
      <if_no>Start with source identification to locate relevant areas</if_no>
    </decision_tree>
    <example>
      find_symbol: Locate specific symbols by name
      get_symbols_overview: Understand file structure
      find_referencing_symbols: Trace dependencies
      Grep: Find patterns across codebase
    </example>
  </pattern>

  <pattern name="synthesis">
    <description>Synthesize findings, each tagged by evidence tier rather than scored</description>
    <decision_tree name="when_to_use">
      <question>Have you collected sufficient evidence from multiple sources?</question>
      <if_yes>Apply synthesis to combine findings, each carrying its evidence tier</if_yes>
      <if_no>Continue evidence collection; name what remains unexamined before concluding</if_no>
    </decision_tree>
    <example>
      Combine evidence from multiple sources
      Tag each finding verified, inferred, or assumed (evidence_standards)
      Report coverage as what was examined and what was not, with the query that bounded the search
      Identify and document information gaps
    </example>
  </pattern>

  <pattern name="reproduce">
    <description>Confirm the issue is reproducible</description>
    <decision_tree name="when_to_use">
      <question>Is this a bug or unexpected behavior investigation?</question>
      <if_yes>Apply reproduce pattern to confirm issue before debugging</if_yes>
      <if_no>Use other investigation patterns for analysis tasks</if_no>
    </decision_tree>
    <example>
      Gather exact steps to reproduce
      Identify environment conditions
      Determine consistency (always/sometimes fails)
    </example>
  </pattern>

  <pattern name="isolate">
    <description>Narrow down the problem scope</description>
    <decision_tree name="when_to_use">
      <question>Is the bug reproducible but involves many components?</question>
      <if_yes>Apply isolate pattern to narrow down the problem scope</if_yes>
      <if_no>Proceed to investigate pattern if scope is clear</if_no>
    </decision_tree>
    <example>
      Identify when issue started (git bisect if needed)
      Remove unrelated components
      Create minimal reproduction case
    </example>
  </pattern>

  <pattern name="non_stationary_symptom">
    <description>A bisection boundary that moves between runs invalidates the observation, not the hypothesis. Bisection — over commits, over input size, over a file's forms — assumes a deterministic oracle. Once the boundary shifts under re-probing, every subsequent narrowing step is fitting noise.</description>
    <decision_tree name="when_to_use">
      <question>Did the boundary you just narrowed to differ from the previous run's boundary?</question>
      <if_yes>Stop narrowing and fix the measurement environment first</if_yes>
      <if_no>Continue isolating, but re-confirm the boundary before each further reduction</if_no>
    </decision_tree>
    <rule>Before continuing to narrow, re-run the identical probe twice and require the identical boundary. If it moves, the oracle is noisy and the next reduction target is meaningless.</rule>
    <interference_checklist>
      <item>Concurrent instances of the same toolchain competing for CPU — check for other long-lived processes of the tool under test before trusting a timing-sensitive result</item>
      <item>A shared build or artifact cache being written by another session</item>
      <item>Fixed-name temporary files colliding between parallel probe sessions; give probe artifacts a process-unique path</item>
    </interference_checklist>
    <sunk_cost_note>A long log of "the next reduction target is..." entries with no reproducibility re-check is the failure signature. The accumulated narrowing feels like progress and creates pressure to continue, but every entry after the boundary first moved is unusable. Treat a resolved-without-a-fix outcome as confirmation that the symptom was environmental, and record that conclusion so the old reduction notes are not mistaken for live findings.</sunk_cost_note>
  </pattern>

  <pattern name="completion_claim_verification">
    <description>Verify a completion claim against the artifact it produced, never against the summary of it. Applies to your own prior work and to any claim arriving as a summary.</description>
    <rule>When a claim has a machine-readable artifact behind it — a coverage report, a directory listing, a lockfile, a build output — read the artifact. "The temporary directories were removed" and a listing showing them present is exactly the gap a skeptical check exists to close.</rule>
    <rule>Read the number, not the rounding. A coverage figure reported as complete but measuring fractionally below it is hiding a small number of genuinely unexercised branches, and those branches are where the untested behavior lives.</rule>
    <rule>Before accepting that a passing test validates a fix, confirm that test's fixtures route through the changed path. A test whose doubles substitute the component that was fixed passes for reasons unrelated to the fix, and is evidence of nothing.</rule>
    <note>State which tier of verification was actually reached rather than implying the highest one; see execution-workflow for the completion-reporting form.</note>
  </pattern>

  <pattern name="investigate">
    <description>Collect evidence systematically for debugging</description>
    <decision_tree name="when_to_use">
      <question>Has the issue been reproduced and isolated?</question>
      <if_yes>Apply investigate pattern to collect debugging evidence</if_yes>
      <if_no>Complete reproduce and isolate patterns first</if_no>
    </decision_tree>
    <example>
      Examine error messages and stack traces
      Check logs at relevant timestamps
      Use Serena for code path analysis
      Trace data flow through the system
    </example>
  </pattern>

  <pattern name="hypothesize">
    <description>Form and test hypotheses</description>
    <decision_tree name="when_to_use">
      <question>Have you collected sufficient debugging evidence?</question>
      <if_yes>Apply hypothesize pattern to form and test root cause theories</if_yes>
      <if_no>Continue investigate pattern to gather more evidence</if_no>
    </decision_tree>
    <example>
      List possible causes
      Rank by likelihood
      Design tests to confirm/refute each
    </example>
  </pattern>

  <pattern name="fix">
    <description>Implement and verify solution</description>
    <decision_tree name="when_to_use">
      <question>Has a hypothesis been confirmed as the root cause?</question>
      <if_yes>Apply fix pattern to implement and verify solution</if_yes>
      <if_no>Continue hypothesize pattern to test other theories</if_no>
    </decision_tree>
    <example>
      Make minimal targeted change
      Verify fix resolves the issue
      Check for regressions
      Add test to prevent recurrence
    </example>
  </pattern>

  <pattern name="architecture_analysis_before_feature">
    <description>Before adding a feature to an unfamiliar codebase, produce a written architecture analysis that turns exploration into a concrete integration plan. This is the deliverable of the investigation and precedes any implementation.</description>
    <template>
      <section name="existing_patterns">Identify the codebase's governing patterns (state management, event flow, rendering or layering, module boundaries) with file:line evidence.</section>
      <section name="reference_implementation">Find the existing feature that most resembles the one to be added and read it as the template to imitate. A near-neighbor already-solved feature is the strongest guide to the codebase's conventions.</section>
      <section name="integration_points">List the exact file:line locations where new code attaches, and what data each point already has in scope.</section>
      <section name="edge_cases_and_risks">Enumerate edge cases and rank technical risks (low/medium/high), each with a mitigation.</section>
      <section name="change_surface">State explicitly both the files to create or modify AND the files that need no change. Naming the "no change needed" set bounds the blast radius and is as valuable as the change list.</section>
      <section name="effort_and_confidence">Give a phased plan with a rough effort estimate, and state the estimate's evidence tier (verified/inferred/assumed) with the basis for that tier — never a numeric confidence.</section>
      <section name="protected_differences">When the task is to align one project with a reference (a sibling service, a ported module, a second plugin in a family), enumerate the divergences that must survive before starting — a different auth scheme, a fixed rather than configurable endpoint, an extra handler the reference lacks, files the reference has that this project should not. The failure mode of conformance work is over-normalization: erasing a divergence that existed for a reason. Writing the protected list up front converts an implicit judgement call into a checkable constraint, the same way change_surface bounds the blast radius by naming what must not change.</section>
    </template>
    <note>Prefer reusing an existing abstraction over inventing one. If the existing abstraction is fundamentally incompatible with the new requirement, say so explicitly and justify a rewrite rather than forcing an ill-fitting extension.</note>
  </pattern>

  <pattern name="deferred_decision_record">
    <description>When a feature or decision is blocked on an external dependency maturing, record it as a deferred decision instead of leaving an open loop or re-investigating from scratch each time.</description>
    <structure>
      <field name="status_and_dates">The decision, its date, and the next scheduled review date.</field>
      <field name="revisit_conditions">A table of conditions that must ALL hold to unblock, each with a target and a concrete "how to check" (a release page, a changelog, a capability list).</field>
      <field name="review_schedule">A periodic cadence (for example quarterly) plus event triggers (on a dependency release, on renewed demand).</field>
      <field name="plan_when_unblocked">The implementation outline and the reference implementations to follow once unblocked.</field>
      <field name="contingency">What to do if the dependency stalls (seek active forks, choose an alternative approach, or close with an explanation).</field>
      <field name="review_log">An append-only log of each review: date, observed dependency version or state, and outcome.</field>
    </structure>
    <note>Make revisit conditions checkable without re-investigation: name the exact capability (for example a required protocol method) or version threshold, so a future review is a lookup rather than a fresh analysis.</note>
  </pattern>

  <pattern name="dead_code_removal_discipline">
    <description>Remove dead code by confirming it is unreferenced, not by matching tokens</description>
    <rule>Confirm no references with a semantic reference search, not a raw occurrence count: package-qualified names and re-exported symbols make token counting produce false positives.</rule>
    <rule>Search across both source and tests: tests may reference private helpers directly, so a source-only search can wrongly mark a symbol dead.</rule>
    <rule>Treat build-system definitions (component or module manifests, barrel or index files) as the boundary of the removal: deleting a file cleanly usually requires updating the manifest that lists it.</rule>
    <rule>After removal, reload or build the affected unit (load the system, run the type or compile check, run the relevant test slice) to prove nothing dangles.</rule>
    <rule>An unused-code warning on a symbol registered through an attribute macro, a plugin registry, or a foreign-function export is boundary noise, not proof: the compiler does not treat the generated registration path as a call site, so the true caller lies outside the language's reference graph. Verify the registration or export path before deleting.</rule>
    <candidates>
      <candidate>Thin compatibility barrels that only re-export concrete modules: remove by pointing each consumer at the concrete module, then delete the barrel.</candidate>
      <candidate>Single-use private helpers whose only call sites are local: inline them, keeping the public entry point as the sole behavioral surface.</candidate>
    </candidates>
  </pattern>
</patterns>

<best_practices>
  <practice priority="critical">Always provide file:line references for all findings using format path/to/file.ext:line_number</practice>
  <practice priority="critical">Tag every investigation finding with its evidence tier (verified/inferred/assumed) and state coverage as what was examined and what was not, with the query that bounded the search (evidence_standards)</practice>
  <practice priority="critical">Complete investigation before proposing solutions</practice>
  <practice priority="high">Use Serena symbol tools before reading entire files</practice>
  <practice priority="high">Independently verify claims rather than confirming assumptions</practice>
  <practice priority="high">Document information gaps and unclear points</practice>
  <practice priority="medium">Check multiple sources when a finding is still inferred, to see whether it can be raised to verified</practice>
  <practice priority="medium">Use systematic debugging phases (reproduce, isolate, investigate, hypothesize, fix)</practice>
  <practice priority="high">Before adding a feature to an unfamiliar codebase, write an architecture analysis: existing patterns, a reference implementation, integration points, risks, and the explicit change surface including files that need no change (architecture_analysis_before_feature)</practice>
  <practice priority="medium">Record work blocked on an external dependency as a deferred decision with checkable revisit conditions, not an open loop (deferred_decision_record)</practice>
  <practice priority="medium">Remove dead code by semantic reference confirmation across source and tests, with a build or reload check after removal (dead_code_removal_discipline)</practice>
  <practice priority="critical">Verify a completion claim against the artifact it produced, and confirm a passing test actually routes through the changed path (completion_claim_verification)</practice>
  <practice priority="high">Re-run a probe twice and require an identical boundary before narrowing further; a moving boundary means the measurement environment is the problem (non_stationary_symptom)</practice>
  <practice priority="medium">When a counter reads zero while the phenomenon is clearly occurring, check whether the instrumentation sits inside a guard (gated_measurement)</practice>
  <practice priority="medium">When aligning a project to a reference, enumerate the divergences that must survive before starting (architecture_analysis_before_feature)</practice>
</best_practices>

<anti_patterns>
  <avoid name="speculation">
    <description>Guessing or making claims when evidence is insufficient</description>
    <instead>Tag the finding `assumed` and state what would confirm it; request additional context if needed</instead>
  </avoid>

  <avoid name="confirming_assumptions">
    <description>Confirming user assumptions without independent verification</description>
    <instead>Independently verify claims by examining code and collecting evidence</instead>
  </avoid>

  <avoid name="uncited_claims">
    <description>Making claims without file:line references</description>
    <instead>Always provide file:line citations for findings using format path/to/file.ext:line_number</instead>
  </avoid>

  <avoid name="premature_implementation">
    <description>Implementing fixes instead of completing analysis</description>
    <instead>Focus on investigation and analysis; provide recommendations without implementation</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Always provide file:line references for all findings using format path/to/file.ext:line_number</rule>
  <rule>Tag every finding with its evidence tier (verified/inferred/assumed) and state coverage as an observable boundary, never as a numeric metric</rule>
  <rule>Complete investigation before proposing solutions</rule>
</rules>

<rules priority="standard">
  <rule>Use Serena symbol tools before reading entire files</rule>
  <rule>Independently verify claims rather than confirming assumptions</rule>
  <rule>Document information gaps and unclear points</rule>
  <rule>Check multiple sources when a finding is still inferred, to see whether it can be raised to verified</rule>
  <rule>Use systematic debugging phases (reproduce, isolate, investigate, hypothesize, fix)</rule>
</rules>

<error_escalation>
  <examples>
    <example severity="low">Evidence trail incomplete</example>
    <example severity="medium">Conflicting evidence found</example>
    <example severity="high">Root cause cannot be determined</example>
    <example severity="critical">Investigation reveals security issue</example>
  </examples>
</error_escalation>

<constraints>
  <must>Build evidence chains before conclusions</must>
  <must>Cite specific file:line references</must>
  <must>Tag every finding's evidence tier explicitly (verified/inferred/assumed)</must>
  <avoid>Speculation without evidence</avoid>
  <avoid>Confirmation bias in hypothesis testing</avoid>
  <avoid>Concluding without exploring alternatives</avoid>
  <avoid>Numeric confidence or coverage scores in place of an evidence tier</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Use for memory operations and symbol-level code navigation</skill>
  <skill name="execution-workflow">Use after investigation to implement fixes with proper delegation</skill>
  <skill name="fact-check">Use to verify external documentation and library behavior</skill>
  <skill name="testing-patterns">Use to add regression tests after fixing identified bugs</skill>
  <skill name="requirements-definition">Use when investigation reveals unclear requirements</skill>
</related_skills>

<related_agents>
  <agent name="explore">Codebase discovery and file-level evidence gathering</agent>
  <agent name="quality-assurance">Cross-check investigation findings for completeness</agent>
  <agent name="validator">Consensus verification when investigation findings conflict</agent>
</related_agents>
