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

<workflow>
  <phase name="prepare">
    <objective>Initialize Serena and establish baseline</objective>
    <step number="1">
      <action>Activate Serena project with activate_project</action>
      <tool>Serena activate_project</tool>
      <output>Project activated</output>
    </step>
    <step number="2">
      <action>Check onboarding status</action>
      <tool>Serena check_onboarding_performed</tool>
      <output>Onboarding status confirmed</output>
    </step>
  </phase>
  <phase name="gather">
    <objective>Collect all memory entries from every layer</objective>
    <step number="1">
      <action>Read CLAUDE.md from project root (if it exists)</action>
      <tool>Read tool</tool>
      <output>Project-wide conventions and instructions</output>
    </step>
    <step number="2">
      <action>Read CLAUDE.local.md from project root (if it exists)</action>
      <tool>Read tool</tool>
      <output>Personal user instructions</output>
    </step>
    <step number="3">
      <action>List all Serena memories</action>
      <tool>Serena list_memories</tool>
      <output>Complete memory list</output>
    </step>
    <step number="4">
      <action>Read each Serena memory</action>
      <tool>Serena read_memory</tool>
      <output>Full memory contents</output>
    </step>
    <step number="5">
      <action>Review auto-memory content already present in system prompt</action>
      <tool>Context analysis</tool>
      <output>Auto-memory inventory</output>
    </step>
    <step number="6">
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
    <step number="1">
      <action>For each memory entry, classify its best destination</action>
      <tool>Analysis</tool>
      <output>Classification table</output>
    </step>
    <step number="2">
      <action>Apply destination criteria: CLAUDE.md for project conventions for all contributors; CLAUDE.local.md for personal instructions for this user only; Serena memory for patterns, decisions, working context; Stay as-is for working notes and temporary context</action>
      <tool>Classification logic</tool>
      <output>Destination assignments</output>
    </step>
    <step number="3">
      <action>Flag ambiguous entries (especially workflow practices) for user input</action>
      <tool>Ambiguity detection</tool>
      <output>Ambiguous entries list</output>
    </step>
  </phase>
  <phase name="identify_cleanup">
    <objective>Find duplicates, outdated entries, and conflicts across all layers</objective>
    <step number="1">
      <action>Identify duplicates: entries already captured elsewhere</action>
      <tool>Cross-layer comparison</tool>
      <output>Duplicate entries with removal proposals</output>
    </step>
    <step number="2">
      <action>Identify outdated entries: entries contradicted by newer entries</action>
      <tool>Chronological analysis</tool>
      <output>Outdated entries with update proposals</output>
    </step>
    <step number="3">
      <action>Identify conflicts: contradictions between layers</action>
      <tool>Conflict detection</tool>
      <output>Conflicting entries with resolution proposals</output>
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
    <serena_validation>
      <tool>think_about_collected_information</tool>
      <trigger>After classification and cleanup analysis completes</trigger>
    </serena_validation>
  </reflection_checkpoint>
  <phase name="present">
    <objective>Compile and present the organized report for user approval</objective>
    <step number="1">
      <action>Group proposals by action type: Promotions, Cleanup, Ambiguous, No action needed</action>
      <tool>Report generation</tool>
      <output>Structured report</output>
    </step>
    <step number="2">
      <action>For each proposal, include source, destination, rationale</action>
      <tool>Report formatting</tool>
      <output>Detailed proposals</output>
    </step>
    <step number="3">
      <action>Present report and await user approval before any modifications</action>
      <tool>User interaction</tool>
      <output>User decisions on proposals</output>
    </step>
  </phase>
  <phase name="failure_handling" inherits="workflow-patterns#failure_handling" />
</workflow>

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

<parallelization inherits="parallelization-patterns#parallelization_readonly" />

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
</decision_criteria>

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

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <low>Minor duplicate between Serena memory and auto-memory</low>
    <medium>Entry destination is ambiguous between CLAUDE.md and CLAUDE.local.md</medium>
    <high>Direct contradiction between memory layers</high>
    <critical>Memory entry contains sensitive information or security-relevant instructions</critical>
  </examples>
</error_escalation>

<related_commands>
  <command name="execute">Apply approved memory changes after review</command>
  <command name="define">When memory review reveals requirements gaps</command>
  <command name="ask">When memory entries raise technical questions</command>
</related_commands>

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
