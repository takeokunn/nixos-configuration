---
argument-hint: ""
description: Memory review and organization command
---

<purpose>
Review the user's memory landscape across all layers (CLAUDE.md, CLAUDE.local.md, Serena memories, auto-memory) and produce a clear report of proposed changes grouped by action type. Do NOT apply changes — present proposals for user approval.
</purpose>
<refs>
  <skill use="patterns">core-patterns</skill>
  <skill use="tools">serena-usage</skill>
</refs>
<rules priority="critical">
  <rule>Present ALL proposals before making any changes</rule>
  <rule>Do NOT modify files without explicit user approval</rule>
  <rule>Do NOT create new files unless target does not exist</rule>
  <rule>Ask about ambiguous entries — do not guess their destination</rule>
  <rule>Keep all operations read-only until user approves specific changes</rule>
</rules>
<rules priority="standard">
  <rule>Delegate file reading and memory gathering to sub-agents</rule>
  <rule>Classify each memory entry by its best destination</rule>
  <rule>Identify duplicates, outdated entries, and conflicts across layers</rule>
  <rule>CLAUDE.md is for instructions to Claude, not user preferences for external tools</rule>
  <rule>Workflow practices are ambiguous — always ask the user about them</rule>
</rules>
<parallelization inherits="parallelization-patterns#parallelization_readonly" />
<ai_principles>
  <inapplicable_traditional_practices>
    <practice>Reading each memory file sequentially before forming a picture — AI can gather all memory layers (CLAUDE.md, CLAUDE.local.md, Serena memories, auto-memory) in a parallel sweep</practice>
    <practice>Guessing the destination for ambiguous workflow practices — AI must flag these for user input rather than assuming, because the boundary between project conventions and personal preferences is context-dependent</practice>
    <practice>Making changes as soon as an issue is identified — the entire proposal set must be assembled and presented before any modification occurs</practice>
  </inapplicable_traditional_practices>
  <applicable_ai_principles>
    <principle>Treat the memory landscape as a graph: read all nodes first, then detect cross-layer duplicates, conflicts, and outdated entries holistically before proposing any changes</principle>
    <principle>Distinguish strictly between CLAUDE.md (shared project conventions) and CLAUDE.local.md (personal user instructions) — misrouting erodes the separation of concerns</principle>
    <principle>Present the complete proposal report in a single structured output before any write operation; user approval is a hard gate, not a preference</principle>
  </applicable_ai_principles>
</ai_principles>
<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and establish baseline</objective>
    <step order="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step order="2">
      <action>Check onboarding status</action>
      <tool>Serena check_onboarding_performed</tool>
      <output>Onboarding status confirmed</output>
    </step>
  </phase>
  <phase name="gather">
    <objective>Collect all memory entries from every layer</objective>
    <step order="1">
      <action>Read CLAUDE.md from project root (if it exists)</action>
      <tool>Read tool</tool>
      <output>Project-wide conventions and instructions</output>
    </step>
    <step order="2">
      <action>Read CLAUDE.local.md from project root (if it exists)</action>
      <tool>Read tool</tool>
      <output>Personal user instructions</output>
    </step>
    <step order="3">
      <action>List all Serena memories</action>
      <tool>Serena list_memories</tool>
      <output>Complete memory list</output>
    </step>
    <step order="4">
      <action>Read each Serena memory</action>
      <tool>Serena read_memory</tool>
      <output>Full memory contents</output>
    </step>
    <step order="5">
      <action>Review auto-memory content already present in system prompt</action>
      <tool>Context analysis</tool>
      <output>Auto-memory inventory</output>
    </step>
    <step order="6">
      <action>Note which team memory sections exist</action>
      <tool>File system check</tool>
      <output>Team memory inventory</output>
    </step>
  </phase>
  <reflection_checkpoint id="gather_complete" after="gather">
    <questions>
      <question weight="0.5">Have all memory layers been read?</question>
      <question weight="0.3">Are any layers missing or inaccessible?</question>
      <question weight="0.2">Is the inventory complete?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Retry missing layer reads or note inaccessible layers</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="classify">
    <objective>Determine the best destination for each memory entry</objective>
    <step order="1">
      <action>For each memory entry, classify its best destination</action>
      <tool>Analysis</tool>
      <output>Classification table</output>
    </step>
    <step order="2">
      <action>Apply destination criteria: CLAUDE.md for project conventions for all contributors; CLAUDE.local.md for personal instructions for this user only; Serena memory for patterns, decisions, working context; Stay as-is for working notes and temporary context</action>
      <tool>Classification logic</tool>
      <output>Destination assignments</output>
    </step>
    <step order="3">
      <action>Flag ambiguous entries (especially workflow practices) for user input</action>
      <tool>Ambiguity detection</tool>
      <output>Ambiguous entries list</output>
    </step>
  </phase>
  <phase name="identify_cleanup">
    <objective>Find duplicates, outdated entries, and conflicts across all layers</objective>
    <step order="1">
      <action>Identify duplicates: entries already captured elsewhere</action>
      <tool>Cross-layer comparison</tool>
      <output>Duplicate entries with removal proposals</output>
    </step>
    <step order="2">
      <action>Identify outdated entries: entries contradicted by newer entries</action>
      <tool>Chronological analysis</tool>
      <output>Outdated entries with update proposals</output>
    </step>
    <step order="3">
      <action>Identify conflicts: contradictions between layers</action>
      <tool>Conflict detection</tool>
      <output>Conflicting entries with resolution proposals</output>
    </step>
    <step order="4">
      <action>Identify potentially stale Serena memories: for each memory, prefer its frontmatter
        last-verified field (see serena-usage#memory_content_format) as the freshness signal;
        flag as a stale candidate if last-verified is more than 3 months old.
        For memories with no frontmatter, fall back to the underlying file's mtime (via a read-only
        filesystem check on .serena/memories/) as the freshness signal instead of parsing the filename —
        filename date suffixes (YYYY-MM) are a naming convention, not a reliability guarantee, and many
        legacy memories carry no date suffix at all. For each stale candidate, verify whether the
        referenced code, pattern, or context still exists in the current codebase.
        Propose one of: re-verify (still valid — bump last-verified, adding frontmatter if absent),
        update (partially outdated), or archive (rename with -archived suffix).</action>
      <tool>Frontmatter analysis (last-verified), file mtime fallback, codebase verification</tool>
      <output>Stale memory candidates with freshness status and proposed action for each</output>
    </step>
  </phase>
  <reflection_checkpoint id="analysis_quality" after="identify_cleanup">
    <questions>
      <question weight="0.4">Have all cross-layer relationships been identified?</question>
      <question weight="0.3">Are cleanup proposals justified with evidence?</question>
      <question weight="0.3">Are ambiguous entries properly flagged rather than assumed?</question>
    </questions>
    <threshold min="70" action="proceed">
      <below_threshold>Re-examine layers for missed relationships</below_threshold>
    </threshold>
  </reflection_checkpoint>
  <phase name="present">
    <objective>Compile and present the organized report for user approval</objective>
    <step order="1">
      <action>Group proposals by action type: Promotions, Cleanup, Ambiguous, No action needed</action>
      <tool>Report generation</tool>
      <output>Structured report</output>
    </step>
    <step order="2">
      <action>For each proposal, include source, destination, rationale</action>
      <tool>Report formatting</tool>
      <output>Detailed proposals</output>
    </step>
    <step order="3">
      <action>Present report and await user approval before any modifications</action>
      <tool>User interaction</tool>
      <output>User decisions on proposals</output>
    </step>
  </phase>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling">
    <step order="1">
      <action>Handle execution errors and apply fallback strategy</action>
      <tool>Error analysis and retry policy</tool>
      <output>Recovered execution path or documented blocker</output>
    </step>
  </phase>
</workflow>

<reflection_checkpoint id="group_consistency">
  <question>Are command-group required sections complete and ordered?</question>
  <question>Is the command safe to execute within stated constraints?</question>
  <threshold>If confidence less than 70, stop and resolve structural gaps first</threshold>
</reflection_checkpoint>
<agents>
  <agent name="explore" subagent_type="explore" readonly="true">Finding CLAUDE.md, CLAUDE.local.md, and other memory files</agent>
  <agent name="general-purpose" subagent_type="general-purpose" readonly="true">Memory classification, duplicate detection, conflict analysis</agent>
  <agent name="validator" subagent_type="validator" readonly="true">Cross-validation of classification decisions</agent>
</agents>
<execution_graph>
  <parallel_group id="gathering" depends_on="none">
    <agent>explore</agent>
  </parallel_group>
  <parallel_group id="analysis" depends_on="gathering">
    <agent>general-purpose</agent>
  </parallel_group>
</execution_graph>
<delegation>
  <requirement>Explicit read-only constraint</requirement>
  <requirement>All memory layers to inspect</requirement>
  <requirement>Classification criteria for destinations</requirement>
</delegation>
<decision_criteria inherits="core-patterns#decision_criteria">
  <criterion name="confidence_calculation">
    <factor name="layer_coverage" weight="0.4">
      <score range="90-100">All accessible memory layers read and analyzed</score>
      <score range="70-89">Most layers read</score>
      <score range="50-69">Some layers missing</score>
      <score range="0-49">Incomplete gathering</score>
    </factor>
    <factor name="classification_quality" weight="0.3">
      <score range="90-100">All entries classified with clear rationale</score>
      <score range="70-89">Most entries classified</score>
      <score range="50-69">Some entries ambiguous</score>
      <score range="0-49">Many entries unclassified</score>
    </factor>
    <factor name="cleanup_completeness" weight="0.3">
      <score range="90-100">All duplicates, outdated, and conflicts identified</score>
      <score range="70-89">Major issues identified</score>
      <score range="50-69">Partial identification</score>
      <score range="0-49">Minimal cleanup analysis</score>
    </factor>
  </criterion>
  <validation_tests>
    <test name="success_case">
      <input>layer_coverage=93, classification_quality=92, cleanup_completeness=92</input>
      <calculation>(93*0.4)+(92*0.3)+(92*0.3) = 92.4</calculation>
      <expected_status>success</expected_status>
      <reasoning>High scores across all factors yield success</reasoning>
    </test>
    <test name="boundary_success_80">
      <input>layer_coverage=80, classification_quality=80, cleanup_completeness=80</input>
      <calculation>(80*0.4)+(80*0.3)+(80*0.3) = 80</calculation>
      <expected_status>success</expected_status>
      <reasoning>Exactly 80 is success threshold</reasoning>
    </test>
    <test name="boundary_warning_79">
      <input>layer_coverage=79, classification_quality=79, cleanup_completeness=79</input>
      <calculation>(79*0.4)+(79*0.3)+(79*0.3) = 79</calculation>
      <expected_status>warning</expected_status>
      <reasoning>79 is below success threshold</reasoning>
    </test>
    <test name="boundary_error_59">
      <input>layer_coverage=59, classification_quality=59, cleanup_completeness=59</input>
      <calculation>(59*0.4)+(59*0.3)+(59*0.3) = 59</calculation>
      <expected_status>error</expected_status>
      <reasoning>59 is at error threshold</reasoning>
    </test>
    <test name="error_case">
      <input>layer_coverage=35, classification_quality=45, cleanup_completeness=40</input>
      <calculation>(35*0.4)+(45*0.3)+(40*0.3) = 39.5</calculation>
      <expected_status>error</expected_status>
      <reasoning>Low scores yield error status</reasoning>
    </test>
  </validation_tests>
</decision_criteria>
<output>
  <format>
    <memory_report>
      <section name="promotions">
        <description>Entries to move to a different layer</description>
        <fields>
          <field>Source layer and entry</field>
          <field>Proposed destination</field>
          <field>Rationale for move</field>
        </fields>
      </section>
      <section name="cleanup">
        <description>Duplicates, outdated entries, and conflicts to resolve</description>
        <fields>
          <field>Entry and its current locations</field>
          <field>Issue type (duplicate / outdated / conflict)</field>
          <field>Proposed resolution</field>
        </fields>
      </section>
      <section name="ambiguous">
        <description>Entries needing user input on destination</description>
        <fields>
          <field>Entry content</field>
          <field>Candidate destinations with trade-offs</field>
          <field>Question for user</field>
        </fields>
      </section>
      <section name="no_action_needed">
        <description>Entries that should stay in their current location</description>
        <fields>
          <field>Brief summary of entries confirmed in place</field>
        </fields>
      </section>
    </memory_report>
  </format>
</output>
<enforcement>
  <mandatory_behaviors>
    <behavior id="REM-B001" priority="critical">
      <trigger>Before any file modification</trigger>
      <action>Present complete proposal report and receive explicit user approval</action>
      <verification>User approval recorded before any write operation</verification>
    </behavior>
    <behavior id="REM-B002" priority="critical">
      <trigger>During classification phase</trigger>
      <action>Flag ambiguous entries (especially workflow practices) for user input</action>
      <verification>Ambiguous entries listed in report with questions</verification>
    </behavior>
    <behavior id="REM-B003" priority="critical">
      <trigger>During gather phase</trigger>
      <action>Read all memory layers: CLAUDE.md, CLAUDE.local.md, Serena memories, auto-memory</action>
      <verification>All accessible layers included in inventory</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="REM-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Modifying any file without explicit user approval</action>
      <response>Block operation, present proposal first</response>
    </behavior>
    <behavior id="REM-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Guessing the destination of ambiguous entries</action>
      <response>Ask user for clarification</response>
    </behavior>
    <behavior id="REM-P003" priority="critical">
      <trigger>Always</trigger>
      <action>Creating new files unless the target file does not exist</action>
      <response>Only propose creation when no target file exists</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>
<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor duplicate between Serena memory and auto-memory</example>
    <example severity="medium">Entry destination is ambiguous between CLAUDE.md and CLAUDE.local.md</example>
    <example severity="high">Direct contradiction between memory layers</example>
    <example severity="critical">Memory entry contains sensitive information or security-relevant instructions</example>
  </examples>
</error_escalation>
<related_commands>
  <command name="execute">Apply approved memory changes after review</command>
  <command name="define">When memory review reveals requirements gaps</command>
  <command name="ask">When memory entries raise technical questions</command>
</related_commands>

<related_agents>
  <agent name="explore">Codebase discovery for uncertain implementation details</agent>
  <agent name="quality-assurance">Cross-check result quality before finalization</agent>
  <agent name="validator">Cross-validation when findings may conflict</agent>
</related_agents>
<related_skills>
  <skill name="serena-usage">Core tool for memory read and write operations</skill>
  <skill name="core-patterns">Shared patterns for error escalation and enforcement</skill>
  <skill name="investigation-patterns">Evidence gathering for conflict resolution</skill>
</related_skills>
<constraints>
  <must>Present all proposals before making any changes</must>
  <must>Ask about ambiguous entries rather than guessing</must>
  <must>Read all accessible memory layers during gather phase</must>
  <must>Include source, destination, and rationale for every proposal</must>
  <must>Distinguish between CLAUDE.md (project conventions) and CLAUDE.local.md (personal instructions)</must>
  <avoid>Modifying files without explicit user approval</avoid>
  <avoid>Assuming destination for workflow practice entries</avoid>
  <avoid>Skipping any memory layer during gathering</avoid>
  <avoid>Creating files that already exist</avoid>
</constraints>
