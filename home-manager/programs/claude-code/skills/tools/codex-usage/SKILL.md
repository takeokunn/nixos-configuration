---
name: Codex Usage
description: This skill should be used when the user asks to "use codex", "code generation", "code modification", "refactoring", or needs Codex MCP guidance. Provides Codex tool usage patterns and configuration.
version: 0.2.0
---

<purpose>
  Provide patterns for effective use of Codex MCP tools for code analysis, generation, and modification.
</purpose>

<tools>
  <tool name="mcp__codex__codex">
    <description>Main Codex tool for code analysis and modification</description>
    <params>
      <param name="prompt" required="true">Task description for Codex to execute</param>
      <param name="sandbox">Sandbox mode: read-only, workspace-write, danger-full-access</param>
      <param name="approval-policy">Approval policy: untrusted, on-failure, on-request, never</param>
      <param name="developer-instructions">Additional context injected as developer role message (recommended for project guidelines)</param>
      <param name="base-instructions">Replace Codex's built-in instructions entirely (use sparingly)</param>
      <param name="cwd">Working directory for the session</param>
      <param name="model">Override model name (e.g., gpt-5.2, gpt-5.2-codex)</param>
      <param name="profile">Configuration profile from config.toml</param>
      <param name="config">Runtime config overrides as JSON object</param>
      <param name="compact-prompt">Custom prompt for conversation history compaction</param>
    </params>
    <defaults>
      <sandbox>workspace-write</sandbox>
      <approval-policy>on-failure</approval-policy>
    </defaults>
    <use_case>Code generation, refactoring, complex modifications</use_case>
  </tool>

  <tool name="mcp__codex__codex-reply">
    <description>Continue an ongoing Codex conversation</description>
    <params>
      <param name="prompt" required="true">Follow-up message or clarification</param>
      <param name="threadId" required="true">Thread identifier from previous codex response (structuredContent.threadId)</param>
    </params>
    <use_case>Continue conversation, iterative refinement, provide additional context</use_case>
    <note>Capture threadId from codex response and pass to subsequent codex-reply calls</note>
  </tool>
</tools>

<concepts>
  <concept name="sandbox_modes">
    <description>Codex sandbox configuration options</description>
    <modes>
      <mode name="read-only">Filesystem read access only; network disabled. Use for analysis tasks.</mode>
      <mode name="workspace-write">Write access to workspace directory only (default). Use for most development tasks.</mode>
      <mode name="danger-full-access">Unrestricted filesystem and network access. Use only in isolated environments.</mode>
    </modes>
  </concept>

  <concept name="approval_policies">
    <description>When to require user approval for shell commands</description>
    <policies>
      <policy name="untrusted">Request approval for all operations. Use for sensitive code or unfamiliar projects.</policy>
      <policy name="on-failure">Pause only when commands fail (recommended default). Balances safety and speed.</policy>
      <policy name="on-request">Prompt when specifically requested. Use for automated pipelines with manual checkpoints.</policy>
      <policy name="never">Execute without intervention. Use only in fully trusted, isolated environments.</policy>
    </policies>
  </concept>

  <concept name="developer_instructions">
    <description>Injecting project context without replacing Codex defaults</description>
    <example>
      Use developer-instructions to provide:
      - Project coding standards and conventions
      - Framework-specific guidelines
      - Team preferences for naming, structure, patterns
      - Language version requirements

      Example:
      developer-instructions: "Follow TypeScript strict mode. Use functional components with hooks. Prefer named exports."
    </example>
  </concept>

  <concept name="session_management">
    <description>Managing multi-turn Codex conversations</description>
    <example>
      1. Call mcp__codex__codex with initial prompt
      2. Capture threadId from response (structuredContent.threadId)
      3. Use mcp__codex__codex-reply with threadId for follow-ups
      4. Keep conversations focused; start new sessions for unrelated tasks
    </example>
  </concept>

  <concept name="task_scope">
    <description>Best practices for task scope</description>
    <example>
      Good: "Add error handling to the processUser function"
      Bad: "Refactor the entire authentication system"

      Keep tasks focused:
      - One function at a time
      - One file at a time
      - Clear, specific requirements
    </example>
  </concept>
</concepts>

<patterns>
  <pattern name="code_generation">
    <description>Generate new code with Codex</description>
    <example>
      <step>Define clear requirements</step>
      <step>Specify target location and file</step>
      <step>Use Codex with focused prompt</step>
      <step>Review generated code</step>
      <step>Test changes</step>
    </example>
  </pattern>

  <pattern name="code_modification">
    <description>Modify existing code with Codex</description>
    <example>
      <step>Read target code first</step>
      <step>Understand current implementation</step>
      <step>Define specific changes needed</step>
      <step>Use Codex with context-aware prompt</step>
      <step>Verify changes preserve functionality</step>
    </example>
  </pattern>

  <pattern name="refactoring">
    <description>Refactor code with Codex</description>
    <example>
      <step>Ensure tests exist before refactoring</step>
      <step>Define refactoring goal (extract method, rename, etc.)</step>
      <step>Use Codex for single refactoring operation</step>
      <step>Run tests to verify behavior unchanged</step>
      <step>Repeat for additional refactorings</step>
    </example>
  </pattern>

  <decision_tree name="when_to_use_codex">
    <question>What type of operation is needed?</question>
    <branch condition="Research or analysis">Do NOT use Codex; use Explore agent or Serena</branch>
    <branch condition="Quality verification">Do NOT use Codex; use quality-assurance agent</branch>
    <branch condition="Security verification">Do NOT use Codex; use security agent</branch>
    <branch condition="Test creation">Do NOT use Codex; use test agent</branch>
    <branch condition="Documentation">Do NOT use Codex; use docs agent</branch>
    <branch condition="Code review">Do NOT use Codex; use quality-assurance agent</branch>
    <branch condition="Code generation">Use Codex with clear requirements</branch>
    <branch condition="Code modification">Use Codex with context</branch>
    <branch condition="Refactoring">Use Codex after ensuring tests exist</branch>
  </decision_tree>

  <decision_tree name="codex_vs_basic_tools">
    <question>Can basic tools (Read/Edit/Write) accomplish this?</question>
    <branch condition="Simple single-line edit">Use Edit tool</branch>
    <branch condition="Small multi-line replacement">Use Edit tool</branch>
    <branch condition="New file from scratch">Use Write tool</branch>
    <branch condition="Complex logic generation">Use Codex</branch>
    <branch condition="Multi-step refactoring">Use Codex</branch>
    <branch condition="Code requiring deep understanding">Use Codex</branch>
  </decision_tree>

  <pattern name="developer_instructions_usage">
    <description>Inject project context for consistent code generation</description>
    <example>
      mcp__codex__codex with:
        prompt: "Add input validation to the createUser function"
        developer-instructions: |
          Follow these project conventions:
          - Use Zod for schema validation
          - Return Result types for error handling
          - Log validation errors at debug level
          - Include JSDoc comments for public functions
    </example>
  </pattern>

  <pattern name="iterative_refinement">
    <description>Multi-turn conversation for complex tasks</description>
    <example>
      <step>Initial call: mcp__codex__codex with prompt and developer-instructions</step>
      <step>Capture threadId from response.structuredContent.threadId</step>
      <step>Review generated code</step>
      <step>If adjustments needed: mcp__codex__codex-reply with threadId and refinement prompt</step>
      <step>Repeat until satisfactory</step>
    </example>
  </pattern>
</patterns>

<workflow>
  <phase name="evaluate">
    <objective>Determine if Codex is the appropriate tool</objective>
    <step order="1">
      <action>Check if basic tools (Read/Edit/Write) can accomplish the task</action>
      <decision>If yes, use basic tools instead of Codex</decision>
    </step>
    <step order="2">
      <action>Verify task is code generation/modification (not research/verification)</action>
      <decision>If research/verification, use Explore agent or Serena MCP</decision>
    </step>
    <step order="3">
      <action>Confirm task scope is single-file, single-task</action>
      <decision>If multi-file, split into separate Codex calls</decision>
    </step>
  </phase>
  <phase name="prepare">
    <objective>Gather context and define requirements</objective>
    <step order="1">
      <action>Read target code using Read tool</action>
      <output>Understanding of current implementation</output>
    </step>
    <step order="2">
      <action>Identify existing patterns and conventions</action>
      <output>Context for developer-instructions</output>
    </step>
    <step order="3">
      <action>Define clear, specific requirements</action>
      <output>Focused prompt ready for Codex</output>
    </step>
  </phase>
  <phase name="execute">
    <objective>Call Codex with prepared context</objective>
    <step order="1">
      <action>Call mcp__codex__codex with prompt and optional developer-instructions</action>
      <output>Generated code</output>
    </step>
    <step order="2">
      <action>Capture threadId if iterative refinement may be needed</action>
      <output>Session identifier for follow-up</output>
    </step>
    <step order="3">
      <action>If refinement needed, use mcp__codex__codex-reply with threadId</action>
      <output>Refined code</output>
    </step>
  </phase>
  <phase name="review">
    <objective>Verify and validate generated code</objective>
    <step order="1">
      <action>Review generated code for correctness</action>
      <output>Validation status</output>
    </step>
    <step order="2">
      <action>Verify code follows project conventions</action>
      <output>Style compliance</output>
    </step>
    <step order="3">
      <action>Test changes to ensure functionality</action>
      <output>Test results</output>
    </step>
  </phase>
</workflow>

<anti_patterns>
  <avoid name="multi_file_single_call">
    <description>Attempting to modify multiple files in single Codex call</description>
    <instead>Make separate Codex calls for each file</instead>
  </avoid>

  <avoid name="vague_prompts">
    <description>Using vague or open-ended prompts</description>
    <instead>Provide specific, actionable requirements</instead>
  </avoid>

  <avoid name="codex_for_research">
    <description>Using Codex for research or analysis tasks</description>
    <instead>Use Explore agent or Serena MCP for investigation</instead>
  </avoid>

  <avoid name="codex_for_verification">
    <description>Using Codex for code review or security checks</description>
    <instead>Use quality-assurance or security agents</instead>
  </avoid>

  <avoid name="skipping_tests">
    <description>Using Codex for refactoring without test coverage</description>
    <instead>Ensure tests exist before refactoring</instead>
  </avoid>
</anti_patterns>

<best_practices>
  <practice priority="critical">Prefer basic tools (Read/Edit/Write) when sufficient</practice>
  <practice priority="critical">One clear, small task per Codex call</practice>
  <practice priority="critical">Separate phases: research -> design -> implementation</practice>
  <practice priority="critical">No multi-file edits in single Codex call</practice>
  <practice priority="high">Provide context about existing code patterns</practice>
  <practice priority="high">Specify expected output format or structure</practice>
  <practice priority="medium">Use codex-reply for iterative refinement</practice>
</best_practices>

<rules priority="critical">
  <rule>Never use Codex for research or analysis tasks</rule>
  <rule>Never use Codex for verification tasks (quality, security, tests)</rule>
  <rule>Always prefer basic tools when they can accomplish the task</rule>
  <rule>One file per Codex call maximum</rule>
</rules>

<rules priority="standard">
  <rule>Provide clear, specific prompts with requirements</rule>
  <rule>Include context about existing code patterns</rule>
  <rule>Test changes after Codex modifications</rule>
  <rule>Use codex-reply for follow-up adjustments</rule>
</rules>

<enforcement>
  <mandatory_behaviors>
    <behavior id="CODEX-B001" priority="critical">
      <trigger>Before using Codex</trigger>
      <action>Verify task is appropriate for Codex (not research/verification)</action>
      <verification>Task type confirmed as code generation/modification</verification>
    </behavior>
    <behavior id="CODEX-B002" priority="critical">
      <trigger>When using Codex</trigger>
      <action>Provide focused, single-task prompt</action>
      <verification>Prompt addresses one file, one task</verification>
    </behavior>
  </mandatory_behaviors>
  <prohibited_behaviors>
    <behavior id="CODEX-P001" priority="critical">
      <trigger>Always</trigger>
      <action>Using Codex for research or analysis</action>
      <response>Use Explore agent or Serena MCP instead</response>
    </behavior>
    <behavior id="CODEX-P002" priority="critical">
      <trigger>Always</trigger>
      <action>Multi-file edits in single Codex call</action>
      <response>Split into separate calls per file</response>
    </behavior>
  </prohibited_behaviors>
</enforcement>

<error_escalation>
  <level severity="low">
    <example>Codex generates code with minor style differences</example>
    <action>Note in report, adjust style manually</action>
  </level>
  <level severity="medium">
    <example>Codex output requires significant adjustments</example>
    <action>Use codex-reply for refinement or switch to Edit tool</action>
  </level>
  <level severity="high">
    <example>Codex fails to understand requirements</example>
    <action>STOP, break task into smaller pieces, rephrase requirements</action>
  </level>
  <level severity="critical">
    <example>Codex generates potentially harmful code</example>
    <action>BLOCK operation, require explicit user review</action>
  </level>
</error_escalation>

<related_skills>
  <skill name="serena-usage">Alternative for code navigation and analysis; use for research tasks</skill>
  <skill name="context7-usage">Fetch library documentation before code generation</skill>
  <skill name="core-patterns">Shared enforcement and escalation patterns</skill>
  <skill name="execution-workflow">Task delegation patterns</skill>
  <skill name="testing-patterns">Ensure test coverage before refactoring</skill>
  <skill name="investigation-patterns">Research methodology before implementation</skill>
</related_skills>

<constraints>
  <must>Verify task is appropriate for Codex before use</must>
  <must>Provide focused, single-file, single-task prompts</must>
  <must>Test changes after Codex modifications</must>
  <avoid>Using Codex for research or analysis</avoid>
  <avoid>Multi-file edits in single call</avoid>
  <avoid>Vague or open-ended prompts</avoid>
</constraints>
