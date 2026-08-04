---
argument-hint: [question]
description: Question and inquiry command
---

<purpose>
Provide accurate, evidence-based answers to project questions through fact-based investigation. Operates in read-only mode; never modifies files.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">investigation-patterns</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
</refs>
<rules priority="critical">
  <rule>NEVER modify, create, or delete files</rule>
  <rule>NEVER implement fixes; provide analysis and suggestions only</rule>
  <rule>ALWAYS base answers on factual investigation from code and documentation</rule>
  <rule>ALWAYS tag each finding verified, inferred, or assumed (core-patterns#evidence_tiers) and state unclear points honestly</rule>
  <rule>NEVER justify user assumptions; prioritize technical accuracy</rule>
</rules>
<rules priority="standard">
  <rule>Use investigation-patterns skill for systematic analysis</rule>
  <rule>Delegate to appropriate agents in parallel</rule>
  <rule>Provide file:line references for all findings</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Investigating files one at a time before synthesizing — AI can survey all relevant files in a single parallel investigation pass</practice>
    <practice>Accepting the question framing as the correct framing — AI should verify whether the stated question matches the underlying need before answering</practice>
    <practice>Reporting uncertainty without evidence — every finding must be anchored to a specific file:line reference, not general impressions</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Map the full evidence surface (all relevant files, cross-references, documentation) before forming any conclusion</principle>
    <principle>Distinguish facts (from code evidence) from inferences (deduced) from speculation (no evidence) — label each finding explicitly</principle>
    <principle>Always verify claimed patterns exist in the current codebase; memory and training data about past states can be stale</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Full memory index</output>
    </step>
    <step order="3">
      <action>Classify task type as "investigation". Apply memory_reading_by_task_type filter
        (serena-usage skill): prioritize {domain}-patterns → architecture-* → {project}-conventions.
        Filter the memory index from step 2 against these categories; record matched names.</action>
      <tool>serena-usage#memory_reading_by_task_type (reference only)</tool>
      <output>Filtered priority memory list for investigation tasks</output>
    </step>
    <step order="4">
      <action>Load only memories matching the prioritized categories with read_memory;
        skip categories absent from the index</action>
      <tool>Serena read_memory</tool>
      <output>Prioritized patterns loaded</output>
    </step>

  </phase>
  <phase name="analyze">
    <step order="1">
      <action>Restate the user's core question in one sentence, and name the claim that would answer it</action>
      <output>Restated question</output>
    </step>
    <step order="2">
      <action>Locate the code and documentation that could bear on the question</action>
      <tool>Glob, Grep, Serena get_symbols_overview</tool>
      <output>Candidate file list</output>
    </step>
    <step order="3">
      <action>State the investigation boundary: what will be read and what is deliberately out of scope</action>
      <output>Scope boundary</output>
    </step>
    <step order="4">
      <action>Classify question type (architecture, implementation, debugging, design)</action>
      <output>Question type, which selects the agents below</output>
    </step>
  </phase>
  <phase name="investigate">
    <step order="1">
      <action>Delegate to explore agent: find relevant files and codebase structure</action>
      <tool>Task tool (explore)</tool>
      <output>File paths and code excerpts with file:line</output>
    </step>
    <step order="2">
      <action>Delegate to design agent: evaluate architecture and component relationships</action>
      <tool>Task tool (design)</tool>
      <output>Architecture analysis and dependency map</output>
    </step>
    <step order="3">
      <action>Delegate to performance agent: identify performance-related aspects (if applicable)</action>
      <tool>Task tool (performance)</tool>
      <output>Bottleneck locations, or an explicit "not applicable"</output>
    </step>
    <step order="4">
      <action>Use fact-check skill patterns: verify external references via Context7 and WebSearch</action>
      <tool>Context7 MCP, WebSearch</tool>
      <output>Verified external claims, flagged claims</output>
    </step>
  </phase>
  <reflection_checkpoint id="investigation_quality">
    <gate>Answer each check with a concrete artifact. A bare "yes" does not clear the gate.</gate>
    <check>Name the files read and the specific lines the answer will rest on.</check>
    <check>Name each agent dispatched and the one claim it returned, or say it returned nothing usable.</check>
    <check>Name any point where two agents disagree, and which one cited a file:line.</check>
    <on_unmet>Widen the investigation or re-dispatch the agent with the specific paths. If only the user
      can settle it, ask with AskUserQuestion rather than picking a reading.</on_unmet>
  </reflection_checkpoint>
  <phase name="synthesize">
    <step order="1">
      <action>Delegate to quality-assurance agent: evaluate code quality findings</action>
      <tool>Task tool (quality-assurance)</tool>
      <output>Issue list with severity and file:line</output>
    </step>
    <step order="2">
      <action>Delegate to code-quality agent: analyze complexity metrics</action>
      <tool>Task tool (code-quality)</tool>
      <output>Complexity metrics and refactoring candidates</output>
    </step>
    <step order="3">
      <action>Compile agent findings, tagging each verified, inferred, or assumed per
        core-patterns#evidence_tiers; downgrade any verified claim that cannot name a command or file:line</action>
      <output>Tagged finding list</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Detect and classify failures during command execution</action>
      <tool>Error analysis and severity assessment</tool>
      <output>Failure classification and impact summary</output>
    </step>
    <step order="2">
      <action>Apply recovery path or escalate with concrete blocker details</action>
      <tool>Retry policy and fallback strategy</tool>
      <output>Recovered flow or explicit blocker report</output>
    </step>
  </phase>
  <phase name="persist">
    <objective>Capture reusable architectural insights to Serena memory</objective>
    <step order="1">
      <action>Evaluate memory_auto_creation_triggers: did this investigation reveal an architectural pattern,
        a significant convention, or a reusable design insight?
        Call list_memories to check if a memory for this topic already exists.</action>
      <tool>Serena list_memories, evaluation against trigger list</tool>
      <output>Trigger match: yes/no; existing memory: yes/no</output>
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
  <check>State that no file was modified — this command is read-only — or name the file that was.</check>
  <on_unmet>Resolve the structural gap before returning the answer.</on_unmet>
</reflection_checkpoint>
<agents>
  <agent name="explore" subagent_type="explore" readonly="true">
    <role>Discover and map codebase structure relevant to the question</role>
    <receives>question_topic, suspected_file_paths[], search_keywords[]</receives>
    <produces>file_paths[], code_excerpts[]{path: file:line, content}, structure_summary</produces>
    <done_when>All relevant files and code paths identified, each with a file:line; anything searched for and not found is reported as not found</done_when>
  </agent>
  <agent name="design" subagent_type="design" readonly="true">
    <role>Evaluate system design, architectural decisions, and component relationships</role>
    <receives>component_names[], question_context, file_paths[]</receives>
    <produces>architecture_analysis, dependency_map, design_assessment{pattern, rationale, alternatives[]}</produces>
    <done_when>Architectural relationships mapped; all relevant components evaluated</done_when>
  </agent>
  <agent name="performance" subagent_type="performance" readonly="true">
    <role>Identify performance characteristics, bottlenecks, and optimization opportunities</role>
    <receives>code_paths[], performance_concern, context</receives>
    <produces>bottleneck_locations[]{file:line, description}, complexity_analysis, optimization_candidates[]</produces>
    <done_when>Performance-sensitive code paths analyzed; findings reported with file:line evidence</done_when>
  </agent>
  <agent name="quality-assurance" subagent_type="quality-assurance" readonly="true">
    <role>Evaluate code quality, best practices compliance, and correctness</role>
    <receives>file_paths[], code_excerpts[], quality_dimensions[]</receives>
    <produces>quality_assessment{issues[]{severity, location: file:line, description}}, gaps[]</produces>
    <done_when>All provided files assessed; every issue carries a file:line</done_when>
  </agent>
  <agent name="code-quality" subagent_type="code-quality" readonly="true">
    <role>Analyze code complexity metrics and structural maintainability</role>
    <receives>file_paths[], complexity_threshold</receives>
    <produces>complexity_metrics{cyclomatic, cognitive}, refactoring_candidates[], maintainability_notes{hotspots[]{file:line}, rationale}</produces>
    <done_when>Complexity metrics computed for all provided files; candidates ranked by impact</done_when>
  </agent>
</agents>
<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>performance</agent>
  </parallel_group>
  <parallel_group id="synthesis" depends_on="investigation">
    <agent>quality-assurance</agent>
    <agent>code-quality</agent>
  </parallel_group>
</execution_graph>
<decision_criteria inherits="core-patterns#decision_criteria">
  <factor name="evidence_quality" precedence="1">
    <unmet>A claim in the answer names no file:line and no command whose output shows it. Read the source
      and cite it, or tag the claim inferred or assumed and say what would confirm it.</unmet>
  </factor>
  <factor name="answer_completeness" precedence="2">
    <unmet>Part of the question is unanswered. Investigate it, or list it under unclear_points with the
      reason it was not answered — never let it drop silently.</unmet>
  </factor>
  <factor name="source_verification" precedence="3">
    <unmet>An external claim (library behavior, API contract, version support) rests on recall rather than
      on Context7 or the vendored source. Verify it before stating it as fact.</unmet>
  </factor>
  <resolution>Apply in precedence order. The first factor whose `unmet` condition holds decides what
    happens next; later factors are not consulted.</resolution>
</decision_criteria>
<output>
  <format>
    <question>Restate the user's question for confirmation</question>
    <investigation>Evidence-based findings with file:line references
- Source 1: `path/to/file.ts:42` - finding
- Source 2: `path/to/other.ts:15` - finding</investigation>
    <conclusion>Direct answer based on evidence</conclusion>
    <evidence_tiers>Each finding tagged verified | inferred | assumed (core-patterns#evidence_tiers), with the file:line or command that backs it</evidence_tiers>
    <recommendations>Optional: Suggested actions without implementation</recommendations>
    <unclear_points>Information gaps that would improve the answer</unclear_points>
    <self_feedback>
      <verification>The command(s) actually run and their exit status, or "none run"</verification>
      <downgrades>Any claim first written as verified that could not name a command or file:line, and the tier it was moved to</downgrades>
      <weakest_claim>The finding resting on the thinnest evidence, and what would confirm it</weakest_claim>
      <gaps>Anything the question asked for that this answer does not address, and why — not attempted, blocked, or out of scope</gaps>
    </self_feedback>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="ASK-B001" priority="critical">
      <trigger>When answering questions</trigger>
      <action>Cite specific file:line references</action>
      <verification>References included in answer</verification>
    </behavior>
    <behavior id="ASK-B002" priority="critical">
      <trigger>When a claim was not directly observed</trigger>
      <action>Tag it inferred or assumed per core-patterns#evidence_tiers and state what would confirm it</action>
      <verification>Every finding in the answer carries a tier; no verified finding lacks a file:line or command</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="ASK-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Answering without code investigation</action>
      <response>Block answer, require investigation first</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor inconsistency in documentation or comments</example>
    <example severity="medium">Unclear code pattern or ambiguous architecture</example>
    <example severity="high">Conflicting evidence about system behavior</example>
    <example severity="critical">Potential security vulnerability or data integrity issue</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="bug">When investigating error-related questions</command>
  <command name="define">When question requires requirements clarification</command>
  <command name="execute">When answer leads to implementation needs</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="investigation-patterns">Core skill for systematic evidence-based analysis</skill>
  <skill name="serena-usage">Symbol-level search for efficient code navigation</skill>
  <skill name="context7-usage">Verify library documentation for accuracy</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>
<constraints>
  <must>Keep all operations read-only</must>
  <must>Provide file:line references for findings</must>
  <must>Tag every finding verified, inferred, or assumed, and name the evidence</must>
  <must>Report the verification actually run, or "none run"</must>
  <avoid>Implementing or modifying any code</avoid>
  <avoid>Guessing when evidence is insufficient</avoid>
  <avoid>Confirming user assumptions without verification</avoid>
</constraints>
