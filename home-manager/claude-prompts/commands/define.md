---
argument-hint: [message]
description: Requirements definition command
allowed-tools: Read, Grep, Glob, Bash, mcp__plugin_claude-code-home-manager_serena__activate_project, mcp__plugin_claude-code-home-manager_serena__check_onboarding_performed, mcp__plugin_claude-code-home-manager_serena__list_memories, mcp__plugin_claude-code-home-manager_serena__read_memory, mcp__plugin_claude-code-home-manager_serena__find_symbol, mcp__plugin_claude-code-home-manager_serena__get_symbols_overview, mcp__plugin_claude-code-home-manager_serena__find_file, mcp__plugin_claude-code-home-manager_serena__search_for_pattern, mcp__plugin_claude-code-home-manager_serena__list_dir, mcp__plugin_claude-code-home-manager_context7__resolve-library-id, mcp__plugin_claude-code-home-manager_context7__get-library-docs, AskUserQuestion, Agent, TaskCreate, TaskUpdate, WebSearch
---

<purpose>
Conduct detailed requirements definition before implementation, clarifying technical constraints, design policies, and specifications.
</purpose>

<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="workflow">requirements-definition</skill>
  <skill use="workflow">fact-check</skill>
  <skill use="tools">serena-usage</skill>
  <skill use="tools">context7-usage</skill>
  <skill use="workflow">define-core</skill>
</refs>

<scope>
  <when_to_use>
    <case>Unclear scope with multiple design choices (e.g., "add authentication", "refactor data layer")</case>
    <case>Multi-component changes spanning 3+ files or 2+ system layers</case>
    <case>Irreversible or high-risk changes (schema migrations, API breaking changes, auth rework)</case>
    <case>Before large implementations where misunderstanding = wasted work</case>
    <case>When the user's request has ambiguous framing that may not reflect the real need</case>
  </when_to_use>
  <when_not_to_use>
    <case>Simple bug with a clear, isolated cause — use /bug instead</case>
    <case>Minor one-line changes or already fully-specified tasks — use /execute instead</case>
    <case>Already-defined requirements that just need technical investigation — use /ask instead</case>
    <case>Documentation-only changes — use /markdown instead</case>
  </when_not_to_use>
</scope>

<rules priority="critical">
  <rule>Never modify, create, or delete files</rule>
  <rule>Never implement code; requirements definition only</rule>
  <rule>Clearly identify technically impossible requests</rule>
  <rule>Prioritize technical validity over user preferences</rule>
  <rule>Technical evidence over speculation</rule>
  <rule>Challenge the user's framing — the stated problem may not be the real problem</rule>
  <rule>Form hypotheses before concluding; signal detection → hypothesis → verification → conclusion</rule>
</rules>

<rules priority="standard">
  <rule>Use requirements-definition skill for methodology</rule>
  <rule>Delegate investigations to sub-agents</rule>
  <rule>Ask questions without limit until requirements are clear</rule>
  <rule>Investigate and question before concluding</rule>
  <rule>Always include a (Recommended) option when presenting choices via AskUserQuestion</rule>
  <rule>Think in Why → How → What order (Golden Circle); output may differ, but reasoning must start from Why</rule>
  <rule>Specify only what is necessary; do not over-specify obvious implementation details</rule>
</rules>

<thinking_framework>
  <golden_circle>
    <principle>Always reason in this order: Why (motivation, constraints, goals) → How (approach, design decisions) → What (specific requirements, acceptance criteria). Output structure may vary, but internal reasoning must start from Why.</principle>
  </golden_circle>
  <abstraction_levels>
    <level id="L0" name="System/Architecture">Which systems, services, or components are affected? What cross-cutting concerns exist?</level>
    <level id="L1" name="Data/Schema">What data structures, database schemas, or state changes are involved?</level>
    <level id="L2" name="Interface/API">What APIs, contracts, or public interfaces change?</level>
    <level id="L3" name="Business Logic/Flow">What business rules, processing flows, or state transitions change?</level>
    <level id="L4" name="Implementation Detail">What specific files, functions, or configuration need to change?</level>
  </abstraction_levels>
  <investigation_depth>
    <rule>Start from L0 (big picture) before diving into L4 (details). Never jump to implementation detail before understanding system impact.</rule>
    <rule>Delegate investigation to appropriate sub-agents per level; synthesize findings yourself.</rule>
  </investigation_depth>
</thinking_framework>

<bias_correction>
  <known_bias id="BC-001">
    <pattern>Accepting the user's stated solution as the requirement</pattern>
    <correction>Distinguish the problem (what needs to change) from the proposed solution (how they think it should change). Requirements define the problem; solutions come later.</correction>
  </known_bias>
  <known_bias id="BC-002">
    <pattern>Jumping to implementation-level requirements before understanding system impact</pattern>
    <correction>Always assess L0 (system/architecture impact) before specifying L4 (implementation details).</correction>
  </known_bias>
  <known_bias id="BC-003">
    <pattern>Over-specifying obvious details while under-specifying the hard parts</pattern>
    <correction>Focus specification effort on decision points (irreversible choices, design branches, non-obvious constraints). Don't document what any competent implementer would naturally do.</correction>
  </known_bias>
  <known_bias id="BC-004">
    <pattern>Treating the first framing as the correct framing</pattern>
    <correction>Rephrase the request in "subject → object → operation" form to expose hidden ambiguity. If the rephrasing feels different from the original, there is a clarification gap.</correction>
  </known_bias>
  <known_bias id="BC-005">
    <pattern>Assuming all capabilities exist before verifying</pattern>
    <correction>Before designing around a framework feature, library, or system capability, verify it exists in the current codebase. Memory about past states may be stale.</correction>
  </known_bias>
</bias_correction>

<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and check existing patterns</objective>
    <step number="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step number="2">
      <action>Check list_memories for relevant patterns</action>
      <tool>Serena list_memories</tool>
      <output>Available memory list</output>
    </step>
    <step number="3">
      <action>Load applicable memories with read_memory</action>
      <tool>Serena read_memory</tool>
      <output>Relevant patterns loaded</output>
    </step>
  </phase>
  <phase name="analyze">
    <objective>Understand the user's request and identify technical constraints</objective>
    <step number="0">
      <action>Get the big picture: which L0 systems/components are affected? Estimate scope (how many layers, files, services). This step prevents diving into details before understanding overall impact.</action>
      <tool>Serena list_dir, find_file, codebase knowledge</tool>
      <output>Affected systems list, scope estimate (small/medium/large), impacted layers</output>
    </step>
    <step number="1">
      <action>Parse user request in "subject → object → operation" form. If the rephrasing differs from the original, flag the gap as a clarification candidate.</action>
      <tool>Text analysis</tool>
      <output>Structured request form, identified framing gaps</output>
    </step>
    <step number="2">
      <action>Identify technical constraints from request context</action>
      <tool>Codebase knowledge</tool>
      <output>Constraint list</output>
    </step>
    <step number="3">
      <action>Determine design decisions requiring user input</action>
      <tool>Requirements analysis</tool>
      <output>Question candidates list</output>
    </step>
    <step number="4">
      <action>Assess technical feasibility at high level</action>
      <tool>Technical knowledge</tool>
      <output>Initial feasibility assessment</output>
    </step>
  </phase>
  <phase name="investigate">
    <objective>Gather evidence from codebase and analyze architecture impact</objective>
    <step number="1">
      <action>Delegate to explore agent: find relevant files and existing patterns</action>
      <tool>Sub-agent delegation</tool>
      <output>File paths, patterns, code samples</output>
    </step>
    <step number="2">
      <action>Delegate to design agent: evaluate architecture consistency and dependencies</action>
      <tool>Sub-agent delegation</tool>
      <output>Architecture analysis, dependency graph</output>
    </step>
    <step number="3">
      <action>Delegate to database agent: analyze database design (if applicable)</action>
      <tool>Sub-agent delegation</tool>
      <output>Schema analysis, query patterns</output>
    </step>
    <step number="4">
      <action>Delegate to general-purpose agent: analyze requirements and estimate effort</action>
      <tool>Sub-agent delegation</tool>
      <output>Effort estimation, risk analysis</output>
    </step>
    <step number="5">
      <action>Use fact-check skill patterns: verify external documentation and standard references via Context7</action>
      <tool>Context7 MCP, WebSearch</tool>
      <output>Verification report, flagged claims</output>
    </step>
  </phase>
  <reflection_checkpoint id="investigation_complete" after="investigate">
    <questions>
      <question weight="0.4">Have all relevant files and patterns been identified?</question>
      <question weight="0.3">Is the scope clearly understood?</question>
      <question weight="0.3">Are there any technical blockers identified?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Expand investigation scope or ask user</below_threshold>
    </threshold>
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After investigation phase completes</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <reflection_checkpoint id="analysis_quality" inherits="workflow-patterns#reflection_checkpoint" />
  <phase name="clarify">
    <objective>Resolve ambiguities through structured user interaction</objective>
    <step number="1">
      <action>Score questions by: design branching, irreversibility, investigation impossibility, effort impact (1-5 each)</action>
      <tool>Question scoring algorithm</tool>
      <output>Prioritized question list</output>
    </step>
    <step number="2">
      <action>Classify questions: spec confirmation, design choice, constraint, scope, priority</action>
      <tool>Question taxonomy</tool>
      <output>Categorized questions</output>
    </step>
    <step number="3">
      <action>Use AskUserQuestion tool for all user interactions (2-4 structured options per question)</action>
      <tool>AskUserQuestion</tool>
      <output>User responses</output>
    </step>
    <step number="4">
      <action>For follow-up clarifications, continue using AskUserQuestion tool rather than plain text</action>
      <tool>AskUserQuestion</tool>
      <output>Additional user responses</output>
    </step>
    <step number="5">
      <action>Present high-score questions first; do not proceed without clear answers</action>
      <tool>Priority ordering</tool>
      <output>Confirmed requirements</output>
    </step>
  </phase>
  <phase name="verify">
    <objective>Validate user decisions against technical evidence</objective>
    <step number="1">
      <action>Verify constraints from answers using agent findings</action>
      <tool>Cross-reference analysis</tool>
      <output>Validated constraints</output>
    </step>
    <step number="2">
      <action>Check implementations related to chosen approach</action>
      <tool>Code analysis</tool>
      <output>Implementation validation</output>
    </step>
  </phase>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
  <phase name="document">
    <objective>Create comprehensive requirements documentation and task breakdown</objective>
    <step number="1">
      <action>Create comprehensive requirements document (internal investigation findings are separate from the output document)</action>
      <tool>Requirements template</tool>
      <output>Complete requirements specification</output>
    </step>
    <step number="2">
      <action>Break down tasks for /execute handoff</action>
      <tool>Task decomposition</tool>
      <output>Phased task list with dependencies</output>
    </step>
  </phase>
  <phase name="critique">
    <objective>Adversarial review of requirements document for gaps and risks</objective>
    <step number="1">
      <action>Delegate requirements document to validator agent for critique</action>
      <tool>Sub-agent delegation (validator)</tool>
      <aspects>Missing steps, unidentified risks, unclear acceptance criteria, dependency gaps, scope ambiguity</aspects>
      <output>Critique report with identified gaps</output>
    </step>
    <step number="2">
      <action>Evaluate critique findings and incorporate valid feedback</action>
      <tool>Synthesis</tool>
      <output>Updated requirements document addressing critique</output>
    </step>
    <step number="3">
      <action>Flag unresolved critique items as outstanding issues</action>
      <tool>Issue tracking</tool>
      <output>Outstanding issues appended to requirements</output>
    </step>
  </phase>
  <reflection_checkpoint id="critique_quality" after="critique">
    <questions>
      <question weight="0.4">Were critique findings adequately addressed or flagged?</question>
      <question weight="0.3">Are remaining outstanding issues clearly documented?</question>
      <question weight="0.3">Is the final requirements document complete and consistent?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Revisit critique findings or ask user for guidance</below_threshold>
    </threshold>
  </reflection_checkpoint>
</workflow>

<common_investigation_workflows>
  <workflow id="A" name="New Feature Definition">
    <step>1. Identify which L0 systems are affected (new vs. extending existing)</step>
    <step>2. Find similar existing implementations as reference patterns</step>
    <step>3. Map data model changes (L1) and API changes (L2)</step>
    <step>4. Identify acceptance criteria from the user's goal, not their proposed solution</step>
  </workflow>
  <workflow id="B" name="Refactor / Architecture Change">
    <step>1. Map current boundaries, dependencies, and change reasons (change-axis analysis)</step>
    <step>2. Identify what stays the same (stable) vs. what changes (variable)</step>
    <step>3. Detect invariants that must not break across the refactor</step>
    <step>4. Estimate blast radius: how many modules are affected by each design choice</step>
  </workflow>
  <workflow id="C" name="Bug Fix Specification">
    <step>1. Reproduce and confirm the failure mode with evidence from code/logs</step>
    <step>2. Distinguish root cause from symptoms</step>
    <step>3. Identify all places where the same root cause could recur</step>
    <step>4. Specify acceptance criteria as observable behavior, not internal mechanism</step>
  </workflow>
  <workflow id="D" name="Integration / External System">
    <step>1. Verify external system capabilities via Context7 or documentation (don't assume)</step>
    <step>2. Map authentication, rate limits, and error contracts</step>
    <step>3. Identify data translation boundaries (what transforms, what passes through)</step>
    <step>4. Define fallback behavior for external failures</step>
  </workflow>
</common_investigation_workflows>

<agents>
  <agent name="design" subagent_type="design" readonly="true">Architecture consistency, dependency analysis, API design</agent>
  <agent name="database" subagent_type="database" readonly="true">Database design and optimization</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Requirements analysis, estimation, dependency analysis</agent>
  <agent name="explore" subagent_type="explore" readonly="true">Finding relevant files and existing patterns</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation and consensus verification</agent>
</agents>

<execution_graph>
  <parallel_group id="investigation" depends_on="none">
    <agent>explore</agent>
    <agent>design</agent>
    <agent>database</agent>
  </parallel_group>
  <parallel_group id="analysis" depends_on="investigation">
    <agent>general-purpose</agent>
  </parallel_group>
</execution_graph>

<delegation>
  <requirement>Scope overview</requirement>
  <requirement>Target file paths</requirement>
  <requirement>Explicit edit prohibition</requirement>
  <requirement>Sub-agents must use AskUserQuestion tool for any user interactions</requirement>
</delegation>

<parallelization inherits="parallelization-patterns#parallelization_readonly" />

<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="requirement_clarity" weight="0.4">
      <score range="90-100">All requirements clear and documented</score>
      <score range="70-89">Core requirements clear</score>
      <score range="50-69">Some ambiguity remains</score>
      <score range="0-49">Many unclear requirements</score>
    </factor>
    <factor name="technical_feasibility" weight="0.3">
      <score range="90-100">Feasibility confirmed with evidence</score>
      <score range="70-89">Likely feasible</score>
      <score range="50-69">Uncertain feasibility</score>
      <score range="0-49">Likely infeasible</score>
    </factor>
    <factor name="stakeholder_alignment" weight="0.3">
      <score range="90-100">All questions answered by user</score>
      <score range="70-89">Most questions answered</score>
      <score range="50-69">Some questions pending</score>
      <score range="0-49">Many questions unanswered</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>requirement_clarity=95, technical_feasibility=90, stakeholder_alignment=90</input>
      <calculation>(95*0.4)+(90*0.3)+(90*0.3) = 92</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>requirement_clarity=80, technical_feasibility=80, stakeholder_alignment=80</input>
      <calculation>(80*0.4)+(80*0.3)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>requirement_clarity=80, technical_feasibility=78, stakeholder_alignment=78</input>
      <calculation>(80*0.4)+(78*0.3)+(78*0.3) = 78.8</calculation>
      <expected_status>warning</expected_status>
      <reasoning>Score below 80 but above 60 triggers warning</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>requirement_clarity=60, technical_feasibility=58, stakeholder_alignment=58</input>
      <calculation>(60*0.4)+(58*0.3)+(58*0.3) = 58.8</calculation>
      <expected_status>error</expected_status>
      <reasoning>Score below 60 triggers error status</reasoning>
    </test>
    <test name="error_case">
      <input>requirement_clarity=40, technical_feasibility=30, stakeholder_alignment=30</input>
      <calculation>(40*0.4)+(30*0.3)+(30*0.3) = 34</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores across all factors result in error</reasoning>
    </test>
  </validation_tests>
</decision_criteria>

<anti_patterns>
  <pattern id="AP-001" name="Solution acceptance without problem validation">
    Accepting the user's proposed solution as the requirement. Requirements must describe the problem to solve; solutions are proposed during /execute, not /define.
  </pattern>
  <pattern id="AP-002" name="Investigation skip">
    Jumping to requirement documentation without codebase investigation. Every requirement must be grounded in evidence from the existing system.
  </pattern>
  <pattern id="AP-003" name="Implementation-level over-specification">
    Writing requirements that dictate function names, variable names, or algorithmic details. Specify behavior and constraints; leave implementation to the implementer.
  </pattern>
  <pattern id="AP-004" name="Confirmation without challenge">
    Confirming requirements without questioning whether the user's framing is correct. Always probe the Why before accepting the What.
  </pattern>
  <pattern id="AP-005" name="Capability assumption">
    Designing requirements around a library feature or system capability without verifying it exists in the current codebase state.
  </pattern>
  <pattern id="AP-006" name="Completeness theater">
    Writing an exhaustively detailed document that covers obvious things but glosses over the genuinely hard design decisions. Length is not quality.
  </pattern>
  <pattern id="AP-007" name="Partial scope definition">
    Defining requirements for one component while ignoring related components that will be affected. Requirements must account for the full blast radius.
  </pattern>
</anti_patterns>

<enforcement>
  <mandatory_behaviors>
    <behavior id="DEF-B001" priority="critical">
      <trigger>Before requirements documentation</trigger>
      <action>Investigate existing codebase patterns</action>
      <verification>Codebase analysis in output</verification>
    </behavior>
    <behavior id="DEF-B002" priority="critical">
      <trigger>For design decisions</trigger>
      <action>Use AskUserQuestion tool with structured options</action>
      <verification>User responses recorded</verification>
    </behavior>
    <behavior id="DEF-B003" priority="critical">
      <trigger>Before finalizing requirements</trigger>
      <action>Run pre-completion self-check (see completion_conditions)</action>
      <verification>All checklist items answered</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="DEF-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying or creating code files</action>
      <response>Block operation, this is read-only command</response>
    </behavior>
    <behavior id="DEF-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Proceeding without answering critical questions</action>
      <response>Block operation, require clarification first</response>
    </behavior>
    <behavior id="DEF-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Copying investigation findings directly into the requirements document without synthesis</action>
      <response>Internal analysis stays internal. The output must be a synthesized, coherent document — not a dump of agent outputs.</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<output>
  <format>
    <requirements_document>
      <section name="Summary" required="always">One-sentence request, background (Why), expected outcomes</section>
      <section name="Current State" required="always">Existing system, tech stack, affected components</section>
      <section name="Functional Requirements" required="always">FR-001 format (mandatory/optional); behavior-level, not implementation-level</section>
      <section name="Non-Functional Requirements" required="when-applicable">Performance, security, maintainability</section>
      <section name="Technical Specifications" required="always">Design policies, impact scope, key decisions and rationale</section>
      <section name="Architecture Impact" required="when-multi-layer">System diagram (Mermaid) if 2+ layers affected; dependency changes</section>
      <section name="Data / Schema Changes" required="when-applicable">ERD or schema diff if data model changes</section>
      <section name="Interface / API Changes" required="when-applicable">Endpoint table or contract diff if public interfaces change</section>
      <section name="Metrics" required="always">
        <metric name="feasibility">0-100 with evidence</metric>
        <metric name="objectivity">0-100</metric>
      </section>
      <section name="Constraints" required="always">Technical, operational</section>
      <section name="Test Requirements" required="always">Unit, integration, acceptance criteria as observable behavior</section>
      <section name="Outstanding Issues" required="always">Unresolved questions; "none" must be explicitly stated</section>
    </requirements_document>
    <task_breakdown>
      <dependency_graph>Task dependencies visualization (Mermaid preferred for complex graphs)</dependency_graph>
      <phased_tasks>Files, overview, dependencies per phase</phased_tasks>
      <execute_handoff>Decisions made, references, constraints, what /execute must NOT assume</execute_handoff>
    </task_breakdown>
  </format>
  <output_rules>
    <rule>Internal investigation analysis is separate from the output document. Do not paste agent outputs into requirements.</rule>
    <rule>Use Mermaid diagrams and tables to convey structure; prefer visual over prose for architectural impacts.</rule>
    <rule>Abstract → Concrete ordering: system impact before implementation detail.</rule>
    <rule>Outstanding issues must always be written — "none" explicitly stated if truly none.</rule>
    <rule>Test requirements must be expressed as observable behavior, not internal mechanism.</rule>
    <rule>Do not include implementation detail that any competent developer would naturally choose.</rule>
  </output_rules>
</output>

<completion_conditions>
  <pre_completion_checklist>
    <item>Have I verified the user's framing reflects the real problem, not just the stated solution?</item>
    <item>Is every requirement grounded in evidence from the codebase investigation?</item>
    <item>Are design decision rationales documented (why this choice, what alternatives were rejected)?</item>
    <item>Have all acceptance criteria been expressed as observable behavior?</item>
    <item>Is the blast radius (which systems/files are affected) clearly stated?</item>
    <item>Are outstanding issues documented, even if the answer is "none"?</item>
    <item>Does the /execute handoff contain enough context that a fresh implementer could proceed without re-asking?</item>
  </pre_completion_checklist>
  <final_self_check>
    <question>Is there a simpler scope that satisfies the user's core need?</question>
    <question>Have I challenged any assumption the user presented as fixed that might actually be flexible?</question>
    <question>Am I specifying decision points (the genuinely hard parts), not just documenting the obvious?</question>
    <question>Does any part of this document assume a capability that I haven't verified exists?</question>
  </final_self_check>
  <done_when>
    <criterion>All critical questions answered by user or flagged as outstanding</criterion>
    <criterion>Technical feasibility confirmed with codebase evidence</criterion>
    <criterion>Scope bounded and blast radius identified</criterion>
    <criterion>Acceptance criteria expressed as observable behavior</criterion>
    <criterion>Critique phase completed and findings incorporated or flagged</criterion>
    <criterion>Pre-completion checklist passed</criterion>
  </done_when>
</completion_conditions>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <low>Minor ambiguity in non-critical feature detail</low>
    <medium>Unclear requirement or ambiguous scope</medium>
    <high>Technically infeasible request or breaking change</high>
    <critical>Request violates security policy or data integrity</critical>
  </examples>
</error_escalation>

<related_commands>
  <command name="ask">When requirements raise technical questions</command>
  <command name="bug">When defining fix requirements for known issues</command>
  <command name="execute">Handoff point after requirements are defined</command>
  <command name="define-full">Full version with self-evaluation phase</command>
  <command name="simplify">Code cleanup after implementation</command>
</related_commands>

<related_skills>
  <skill name="requirements-definition">Core methodology for specification</skill>
  <skill name="investigation-patterns">Evidence gathering for feasibility</skill>
  <skill name="serena-usage">Check existing patterns and memories</skill>
  <skill name="fact-check">External source verification using Context7 and WebSearch</skill>
</related_skills>

<constraints>
  <must>Keep all operations read-only</must>
  <must>Delegate detailed investigation to sub-agents</must>
  <must>Use AskUserQuestion tool for structured user interactions</must>
  <must>Present questions before making assumptions</must>
  <must>Start every investigation from the big picture (L0) before the detail (L4)</must>
  <must>Run pre-completion checklist before finalizing requirements document</must>
  <avoid>Implementing or modifying code</avoid>
  <avoid>Justifying user requests over technical validity</avoid>
  <avoid>Proceeding without clear answers to critical questions</avoid>
  <avoid>Using plain text output for questions instead of AskUserQuestion tool</avoid>
  <avoid>Specifying implementation details that any competent developer would naturally choose</avoid>
  <avoid>Assuming capabilities exist without verifying in the current codebase</avoid>
</constraints>
