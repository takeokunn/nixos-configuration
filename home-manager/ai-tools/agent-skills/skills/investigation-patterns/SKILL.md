---
name: Investigation Patterns
description: This skill should be used when the user asks to "investigate code", "analyze implementation", "find patterns", "understand codebase", "debug issue", "find bug", "troubleshoot", or needs evidence-based code analysis and debugging. Provides systematic investigation and debugging methodology.
version: 2.1.0
---

<purpose>
  Provide systematic patterns for codebase investigation and debugging, ensuring evidence-based analysis with proper confidence assessment.
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
    <description>Standards for collecting and reporting evidence</description>
    <example>
      Citation: Always provide file:line references (path/to/file.ext:line_number)

      Confidence levels:

      - 90-100: Direct code evidence, explicit documentation
      - 70-89: Strong inference from multiple sources
      - 50-69: Reasonable inference with some gaps
      - 0-49: Speculation, insufficient evidence

      Coverage levels:

      - 90-100: All relevant files examined
      - 70-89: Most relevant files examined
      - 50-69: Key files examined, some gaps
      - 0-49: Limited examination
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
      <metrics>
        - Confidence: 0-100
        - Evidence Coverage: 0-100</metrics>
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
    <description>Synthesize findings with confidence metrics</description>
    <decision_tree name="when_to_use">
      <question>Have you collected sufficient evidence from multiple sources?</question>
      <if_yes>Apply synthesis to combine findings with confidence metrics</if_yes>
      <if_no>Continue evidence collection to increase coverage</if_no>
    </decision_tree>
    <example>
      Combine evidence from multiple sources
      Rate confidence based on evidence quality (0-100)
      Report coverage of relevant code examined (0-100)
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
      <section name="effort_and_confidence">Give a phased plan with a rough effort estimate and a confidence level with its basis.</section>
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
    <candidates>
      <candidate>Thin compatibility barrels that only re-export concrete modules: remove by pointing each consumer at the concrete module, then delete the barrel.</candidate>
      <candidate>Single-use private helpers whose only call sites are local: inline them, keeping the public entry point as the sole behavioral surface.</candidate>
    </candidates>
  </pattern>
</patterns>

<best_practices>
  <practice priority="critical">Always provide file:line references for all findings using format path/to/file.ext:line_number</practice>
  <practice priority="critical">Rate confidence and coverage metrics for all investigation results</practice>
  <practice priority="critical">Complete investigation before proposing solutions</practice>
  <practice priority="high">Use Serena symbol tools before reading entire files</practice>
  <practice priority="high">Independently verify claims rather than confirming assumptions</practice>
  <practice priority="high">Document information gaps and unclear points</practice>
  <practice priority="medium">Check multiple sources to increase confidence</practice>
  <practice priority="medium">Use systematic debugging phases (reproduce, isolate, investigate, hypothesize, fix)</practice>
  <practice priority="high">Before adding a feature to an unfamiliar codebase, write an architecture analysis: existing patterns, a reference implementation, integration points, risks, and the explicit change surface including files that need no change (architecture_analysis_before_feature)</practice>
  <practice priority="medium">Record work blocked on an external dependency as a deferred decision with checkable revisit conditions, not an open loop (deferred_decision_record)</practice>
  <practice priority="medium">Remove dead code by semantic reference confirmation across source and tests, with a build or reload check after removal (dead_code_removal_discipline)</practice>
</best_practices>

<anti_patterns>
  <avoid name="speculation">
    <description>Guessing or making claims when evidence is insufficient</description>
    <instead>Clearly state confidence levels and information gaps; request additional context if needed</instead>
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
  <rule>Rate confidence and coverage metrics for all investigation results</rule>
  <rule>Complete investigation before proposing solutions</rule>
</rules>

<rules priority="standard">
  <rule>Use Serena symbol tools before reading entire files</rule>
  <rule>Independently verify claims rather than confirming assumptions</rule>
  <rule>Document information gaps and unclear points</rule>
  <rule>Check multiple sources to increase confidence</rule>
  <rule>Use systematic debugging phases (reproduce, isolate, investigate, hypothesize, fix)</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
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
  <must>State confidence levels explicitly</must>
  <avoid>Speculation without evidence</avoid>
  <avoid>Confirmation bias in hypothesis testing</avoid>
  <avoid>Concluding without exploring alternatives</avoid>
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
