---
name: Technical Documentation
description: This skill should be used when the user asks to "write documentation", "create README", "API docs", "design document", "specification", "user guide", or needs documentation guidance. Also covers auditing documentation against code in both directions — over-claims and, equally, shipped features still described as future work — confirming a documented capability is actually reachable rather than merely named by a function, publishing a registry of retired names alongside the current names that resemble them so nothing correct gets renamed, and recording the rationale at the change site when a previously-reasoned exception is reversed instead of leaving it in a commit message. Provides documentation patterns for developers, teams, and end-users in both English and Japanese.
version: 2.4.0
---

<purpose>
  Provide structured patterns for creating technical documentation including README, design documents, API specifications, and user guides for diverse audiences.
</purpose>

<tools>
  <tool>Write - Create new documentation files</tool>
  <tool>Edit - Update existing documentation</tool>
  <tool>Read - Review existing documentation and code</tool>
  <tool>Grep - Search for patterns across documentation</tool>
  <tool>Glob - Find related documentation files</tool>
</tools>

<concepts>
  <concept name="document_types">Four primary types: README (project intro), API spec (endpoints/interfaces), design doc (architecture decisions), user guide (end-user tutorials)</concept>
  <concept name="audience_levels">Developer (technical depth), team member (context + depth), end user (no jargon, step-by-step)</concept>
  <concept name="progressive_disclosure">Start with quick start, then common cases, then advanced config, finally edge cases</concept>
  <concept name="documentation_lifecycle">Plan (outline) → Draft (write + examples) → Review (verify accuracy) → Maintain (update with code)</concept>
  <concept name="bidirectional_drift">Documentation rots in two directions: it over-claims (describes capabilities that were never built or were removed) and it under-claims (still calls shipped work "planned"). Teams reliably audit only the first, so the second accumulates unchecked</concept>
  <concept name="documentation_sites">Not all documentation lives in documents. A rationale belongs wherever the reader who needs it will be standing — a conventions file, a design doc, or a comment at the line itself</concept>
</concepts>

<patterns>
  <pattern name="readme_document_type">
    <description>Project introduction and quick start guide</description>
    <decision_tree name="when_to_use">
      <question>Is this the main entry point for project documentation?</question>
      <if_yes>Create README with quick start and overview</if_yes>
      <if_no>Consider using design doc for detailed architecture or user guide for end-user documentation</if_no>
    </decision_tree>
    <context>
      <audience>Developers, contributors, users</audience>
      <when_to_use>Creating or updating main project documentation</when_to_use>
    </context>
    <structure>
      <section>Project title and badges</section>
      <section>One-line description</section>
      <section>Key features (3-5 bullet points)</section>
      <section>Quick start / Installation</section>
      <section>Basic usage example</section>
      <section>Documentation links</section>
      <section>Contributing / License</section>
    </structure>
  </pattern>

  <pattern name="api_specification_document_type">
    <description>API reference documentation</description>
    <decision_tree name="when_to_use">
      <question>Are you documenting API endpoints or SDK interfaces?</question>
      <if_yes>Create API specification with authentication, endpoints, and examples</if_yes>
      <if_no>Use README for library usage or design doc for internal architecture</if_no>
    </decision_tree>
    <context>
      <audience>Developers integrating with the API</audience>
      <when_to_use>Documenting REST APIs, GraphQL schemas, or SDK interfaces</when_to_use>
    </context>
    <structure>
      <section>Overview and authentication</section>
      <section>Base URL and versioning</section>
      <section>Endpoints (method, path, parameters, response)</section>
      <section>Error codes and handling</section>
      <section>Rate limits</section>
      <section>Examples (curl, language-specific)</section>
    </structure>
  </pattern>

  <pattern name="design_document_type">
    <description>Technical design and architecture documentation</description>
    <decision_tree name="when_to_use">
      <question>Are you proposing a major feature or architectural change?</question>
      <if_yes>Create design document with technical details, alternatives, and rollout plan</if_yes>
      <if_no>Use inline code comments for small changes or README for usage instructions</if_no>
    </decision_tree>
    <context>
      <audience>Team members, reviewers, future maintainers</audience>
      <when_to_use>Proposing new features, architectural changes, or major refactors</when_to_use>
    </context>
    <structure>
      <section>Summary (problem, solution, scope)</section>
      <section>Background and motivation</section>
      <section>Goals and non-goals</section>
      <section>Technical design (architecture, data flow)</section>
      <section>Alternatives considered</section>
      <section>Security / Privacy considerations</section>
      <section>Testing strategy</section>
      <section>Rollout plan</section>
    </structure>
  </pattern>

  <pattern name="user_guide_document_type">
    <description>End-user facing documentation</description>
    <decision_tree name="when_to_use">
      <question>Is your audience non-technical end users?</question>
      <if_yes>Create user guide with step-by-step tutorials and troubleshooting</if_yes>
      <if_no>Use API docs for developers or README for contributors</if_no>
    </decision_tree>
    <context>
      <audience>Non-technical users, administrators</audience>
      <when_to_use>Creating help documentation, tutorials, or product guides</when_to_use>
    </context>
    <structure>
      <section>Getting started</section>
      <section>Core concepts</section>
      <section>Step-by-step tutorials</section>
      <section>Feature reference</section>
      <section>Troubleshooting / FAQ</section>
      <section>Glossary</section>
    </structure>
  </pattern>

  <pattern name="readme_structure">
    <description>Standard structure for README documentation</description>
    <example>
      <note>Project Name</note>

      [![Badge](https://img.shields.io/badge/example-badge-blue)]

      Brief one-line description of what the project does.

      <features>
        - Feature 1
        - Feature 2
        - Feature 3
      </features>

      <quick_start>
        ```bash
        npm install package-name
        ```
      </quick_start>

      <basic_usage>
        ```typescript
        import { example } from "package-name";

        const result = example();
        console.log(result);
        ```
      </basic_usage>

      <documentation>
        See [full documentation](link) for detailed guides.
      </documentation>

      <contributing>
        Contributions welcome! See [CONTRIBUTING.md](link).
      </contributing>

      <license>
        MIT
      </license>
    </example>
  </pattern>

  <pattern name="api_documentation">
    <description>Comprehensive API reference documentation structure</description>
    <example>
      <note>API Reference</note>

      <authentication>
        All requests require an API key in the Authorization header:

        ```bash
        Authorization: Bearer YOUR_API_KEY
        ```
      </authentication>

      <base_url>
        ```
        https://api.example.com/v1
        ```
      </base_url>

      <endpoints>
        <get_users>
          Retrieve a list of users.

          **Parameters:**

          - `limit` (integer, optional): Number of results (default: 10)
          - `offset` (integer, optional): Pagination offset (default: 0)

          **Response:**

          ```json
          {
            "users": [
              { "id": 1, "name": "John Doe" },
              { "id": 2, "name": "Jane Smith" }
            ],
            "total": 100
          }
          ```

          **Error Codes:**

          - `401`: Unauthorized - Invalid API key
          - `429`: Rate limit exceeded
        </get_users>
      </endpoints>
    </example>
  </pattern>

  <pattern name="design_document_structure">
    <description>Technical design document format for architectural decisions</description>
    <example>
      <note>Feature Name Design Document</note>

      <summary>
        **Problem:** Brief description of the problem being solved
        **Solution:** High-level approach
        **Scope:** What's included and what's not
      </summary>

      <background>
        Context and motivation for this design.
      </background>

      <goals_and_non_goals>
        **Goals:**

        - Goal 1
        - Goal 2

        **Non-Goals:**

        - What we're explicitly not doing
        - Future considerations
      </goals_and_non_goals>

      <technical_design>
        <architecture>
          [Diagram or description of system architecture]
        </architecture>

        <data_flow>
          1. User action
          2. System processing
          3. Response
        </data_flow>

        <components>
          **Component A:** Responsible for X
          **Component B:** Responsible for Y
        </components>
      </technical_design>

      <alternatives_considered>
        <alternative_1>
          Pros: ...
          Cons: ...
          Decision: Not chosen because...
        </alternative_1>
      </alternatives_considered>

      <security_considerations>
        - Data encryption at rest and in transit
        - Authentication and authorization
        - Input validation
      </security_considerations>

      <testing_strategy>
        - Unit tests for component logic
        - Integration tests for API contracts
        - E2E tests for critical user flows
      </testing_strategy>

      <rollout_plan>
        1. Phase 1: Internal testing
        2. Phase 2: Beta release (10% of users)
        3. Phase 3: Full rollout
      </rollout_plan>
    </example>
  </pattern>

  <pattern name="user_guide_structure">
    <description>End-user documentation with step-by-step instructions</description>
    <example>
      <note>User Guide</note>

      <getting_started>
        Welcome! This guide will help you get started with [Product Name].
      </getting_started>

      <core_concepts>
        **Workspace:** A container for your projects
        **Project:** A collection of related items
        **Item:** The basic unit of work
      </core_concepts>

      <creating_your_first_project>
        1. Click the "New Project" button
        2. Enter a project name
        3. Choose a template (optional)
        4. Click "Create"

        You'll see your new project in the sidebar.
      </creating_your_first_project>

      <troubleshooting>
        <troubleshooting_login>
          1. Check your email address is correct
          2. Click "Forgot Password" to reset
          3. Contact support if the issue persists
        </troubleshooting_login>

        <troubleshooting_data>
          Ensure you have a stable internet connection. The app auto-saves every 30 seconds.
        </troubleshooting_data>
      </troubleshooting>

      <glossary>
        **Term:** Definition
        **Another Term:** Another definition
      </glossary>
    </example>
  </pattern>

  <pattern name="retired_vocabulary_registry">
    <description>A conventions document lists superseded names — and, in the same place, the current names that resemble them</description>
    <decision_tree name="when_to_use">
      <question>Has a rename or refactor left names in circulation that should no longer be used?</question>
      <if_yes>Add a retired-vocabulary section listing each retired term, its replacement, and the still-current names it can be confused with</if_yes>
      <if_no>An ordinary glossary of current terms is sufficient</if_no>
    </decision_tree>
    <context>
      <audience>Contributors and coding agents working from the conventions document</audience>
      <when_to_use>After a vocabulary change large enough that old names persist in memory, old branches, or old examples</when_to_use>
    </context>
    <example>
      <note>Vocabulary</note>

      **Retired — do not reintroduce**

      - `ItemRequest` → use `ItemCommand`
      - `processItem()` → use `applyItem()`

      **Still current, despite the resemblance**

      - `ItemQuery` is current. It is not a retired name and must not be renamed to `ItemCommand`.
      - `processItemEvent()` is current; only the non-event `processItem()` was retired.
    </example>
    <note>The second half is what makes the list safe to act on. Publishing retired terms alone reliably produces over-correction: readers and agents start "fixing" names that merely resemble the retired ones, which turns a documentation aid into a source of regressions.</note>
  </pattern>

  <pattern name="status_claim_audit">
    <description>Auditing an implementation-status or capability document against the code in both directions</description>
    <decision_tree name="when_to_use">
      <question>Does a document assert what is or is not implemented — a status page, a roadmap, a security or compliance capability list?</question>
      <if_yes>Audit every claim in both directions; over-claims and under-claims have different costs and both are present</if_yes>
      <if_no>Ordinary accuracy review against the implementation is enough</if_no>
    </decision_tree>
    <example>
      For each "we do X" claim: locate the implementing symbol, then confirm it is reachable from a live entry point. A function that exists but nothing calls is still an over-claim.
      For each "X is not yet implemented" claim: search for it anyway. Shipped features routinely stay listed as future work.
      For each named mechanism ("over WebSocket", "via the message queue"): verify the mechanism, not just the capability — docs commonly keep the right feature and the wrong transport.
      Record the outcome per claim: confirmed, over-claimed, under-claimed, or wrong-mechanism.
    </example>
    <note>The asymmetry is the point. Over-claims are the dangerous kind — a stale line saying a safety control exists gets cited as evidence that it does. Under-claims are the wasteful kind — they cause already-shipped features to be re-planned and rebuilt. Because reviewers instinctively hunt only for the first, the second is where the surprises are.</note>
  </pattern>

  <pattern name="reversal_rationale_at_change_site">
    <description>When a change deliberately reverses a previously-reasoned exception, the reasoning goes where the exception was recorded</description>
    <decision_tree name="when_to_use">
      <question>Does this change delete, invert, or narrow something that carried a documented rationale — an exemption, a suppression, a deliberate gate, a documented workaround?</question>
      <if_yes>Write the reversal's reasoning at that site, in the same form the original rationale took</if_yes>
      <if_no>The change speaks for itself; do not add a comment restating what the code does</if_no>
    </decision_tree>
    <example>
      Trigger: you are removing a comment, an exemption entry, or a config exception that explained why something was the way it was.
      Where it goes: the same file and line region, not the commit message, the pull request, or a session memory — none of those are visible to the person reading the code a year later.
      What it says: that the removal was intentional and what changed to make the original reason no longer apply. Enough that a reader does not "fix" it back.
      What it does not say: what the surrounding code does, or that the change is correct.
    </example>
    <note>This is the narrow case where a code comment earns its place: an absence carries no evidence, so a deliberately removed exception is indistinguishable from an accidental deletion. Reviewers converge on flagging it as a regression, which is a reliable signal that the reader needs the note more than the author expected.</note>
  </pattern>
</patterns>

<language_guidelines>
  <english>
    <style>Active voice, present tense</style>
    <tone>Professional but approachable</tone>
    <avoid>Unnecessarily complex words, idioms that don't translate</avoid>
    <example>
      <good_example>Good</good_example>
      Run this command to start the server.

      <bad_example>Bad</bad_example>
      The server can be started by running the following command.
    </example>
  </english>

  <japanese>
    <style>です・ます調 (polite form) for user docs, である調 for technical specs</style>
    <tone>丁寧だが簡潔</tone>
    <avoid>過度なカタカナ語、曖昧な表現</avoid>
    <example>
      <good_example>Good</good_example>
      以下のコマンドでサーバーを起動します。

      <bad_example>Bad</bad_example>
      サーバーの起動については、下記コマンドを実行することで可能となります。
    </example>
  </japanese>

  <bilingual>
    <rule>Maintain parallel structure between languages</rule>
    <rule>Keep code examples identical, translate only prose</rule>
    <rule>Use consistent terminology (create glossary if needed)</rule>
  </bilingual>
</language_guidelines>

<output>
  <format>
    <document_plan>
      - Type: [readme/api_spec/design_doc/user_guide]
      - Audience: [developer/team/end_user]
      - Language: [en/ja/both]
    </document_plan>

    <structure>
      [Proposed sections based on document type]
    </structure>

    <content>
      [Actual documentation content]
    </content>

    <review_checklist>
      - [ ] Technical accuracy verified
      - [ ] Code examples tested
      - [ ] Links working
      - [ ] Appropriate for audience
      - [ ] Grammar and spelling checked
    </review_checklist>
  </format>
</output>

<best_practices>
  <practice priority="critical">
    <description>Audience-first approach - Write for your specific audience's knowledge level</description>
    <example>
      Developers: Assume technical background, focus on implementation details
      Team members: Balance context with technical depth
      End users: Avoid jargon, use step-by-step instructions
    </example>
  </practice>

  <practice priority="critical">
    <description>Progressive disclosure - Start with essentials, reveal complexity gradually</description>
    <example>
      1. Quick start for immediate value
      2. Common use cases
      3. Advanced configuration
      4. Edge cases and troubleshooting
    </example>
  </practice>

  <practice priority="high">
    <description>Make content scannable to enable quick information retrieval</description>
    <example>
      - Use descriptive headings
      - Use bullet points for lists
      - Include code blocks with syntax highlighting
      - Use tables for structured data
      - Use bold for key terms (sparingly)
    </example>
  </practice>

  <practice priority="high">
    <description>Example-driven documentation - Show, don't just tell</description>
    <example>
      - Include working code examples
      - Show expected output
      - Provide copy-pasteable commands
    </example>
  </practice>

  <practice priority="medium">
    <description>Active voice and present tense for clarity</description>
    <example>
      Good: Run this command to start the server.
      Bad: The server can be started by running the following command.
    </example>
  </practice>

  <practice priority="medium">
    <description>Test all code examples before publishing</description>
    <example>
      Always verify that code examples compile and run correctly
      Include expected output
      Test edge cases mentioned in documentation
    </example>
  </practice>
</best_practices>

<anti_patterns>
  <avoid name="wall_of_text">
    <description>Long paragraphs without formatting</description>
    <instead>Break into smaller paragraphs, use bullet points, headings, and code blocks</instead>
  </avoid>

  <avoid name="outdated_info">
    <description>Documenting historical context instead of current state</description>
    <instead>Document what exists now, move history to a separate section if needed</instead>
  </avoid>

  <avoid name="assuming_context">
    <description>Using terms or concepts without definition</description>
    <instead>Define terms on first use, link to prerequisites, provide glossary</instead>
  </avoid>

  <avoid name="untested_examples">
    <description>Including code examples that haven't been tested</description>
    <instead>Always verify code examples compile and run correctly before publishing</instead>
  </avoid>

  <avoid name="passive_voice">
    <description>Using passive constructions that obscure agency</description>
    <instead>Use active voice for clarity (e.g., "Run the command" not "The command should be run")</instead>
  </avoid>

  <avoid name="jargon_overload">
    <description>Using technical jargon without explanation</description>
    <instead>Define technical terms on first use, provide a glossary, or use simpler language for user-facing docs</instead>
  </avoid>

  <avoid name="missing_prerequisites">
    <description>Assuming users have required knowledge or setup</description>
    <instead>List prerequisites clearly at the beginning, link to setup guides</instead>
  </avoid>

  <avoid name="vague_instructions">
    <description>Using imprecise language like "simply" or "just" without concrete steps</description>
    <instead>Provide specific, numbered steps with expected outcomes</instead>
  </avoid>

  <avoid name="retired_terms_without_look_alikes">
    <description>Publishing a list of retired names without naming the current names that resemble them, which invites readers and agents to rename things that were never retired</description>
    <instead>Pair every retired-vocabulary list with an explicit "still current despite the resemblance" set</instead>
  </avoid>

  <avoid name="one_directional_status_audit">
    <description>Auditing a status document only for over-claims, leaving shipped features permanently described as future work</description>
    <instead>Check both directions; verify "not yet implemented" claims by searching for the implementation anyway</instead>
  </avoid>

  <avoid name="capability_claim_without_reachability">
    <description>Confirming a documented capability by finding a function with the right name, without checking that anything calls it</description>
    <instead>Trace the symbol to a live entry point; unreachable code makes the claim false in exactly the way that matters</instead>
  </avoid>

  <avoid name="silent_exception_reversal">
    <description>Removing a documented exception, exemption, or workaround and leaving the reasoning only in the commit message, the pull request, or a conversation</description>
    <instead>Record the reversal's rationale at the site the exception occupied, where the next reader will actually be</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Verify all code examples compile and run before including in documentation</rule>
  <rule>Match documentation structure to audience knowledge level</rule>
  <rule>Never publish documentation with placeholder content or TODOs</rule>
</rules>

<rules priority="standard">
  <rule>Use active voice and present tense for instructions</rule>
  <rule>Include expected output for all code examples</rule>
  <rule>Define technical terms on first use or link to glossary</rule>
  <rule>Keep README under 500 lines; link to detailed docs for more</rule>
  <rule>Audit status and capability claims in both directions; verify "not yet implemented" statements against the code, not only "we support X" statements</rule>
  <rule>Trace each capability claim to a live entry point, not merely to a matching symbol</rule>
  <rule>Pair any retired-terminology list with the current names it could be confused with</rule>
  <rule>Record the reasoning for a deliberate reversal at the site the reversed decision occupied</rule>
</rules>

<error_escalation>
  <examples>
    <example severity="low">Minor formatting inconsistency</example>
    <example severity="medium">Outdated information detected</example>
    <example severity="high">Incorrect technical information</example>
    <example severity="critical">Security-sensitive information exposed</example>
  </examples>
</error_escalation>

<constraints>
  <must>Verify accuracy against actual implementation</must>
  <must>Include runnable code examples</must>
  <must>Follow project documentation style</must>
  <avoid>Documenting without reading code</avoid>
  <avoid>Adding timestamps to documents</avoid>
  <avoid>Duplicating information unnecessarily</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Symbol operations for extracting code examples and API signatures</skill>
  <skill name="context7-usage">Library documentation lookup for accurate API references</skill>
  <skill name="investigation-patterns">Analyzing codebases to understand features for documentation</skill>
  <skill name="technical-writing">Creating blog posts and tutorials from documentation; also the canonical source for Japanese prose-quality norms (argumentation rigor, LLM-tell avoidance, dramatization restraint, redundancy) — apply its `&lt;japanese&gt;&lt;prose_norms&gt;` when writing Japanese documentation</skill>
</related_skills>

<related_agents>
  <agent name="docs">Primary agent for documentation generation following this skill's patterns</agent>
  <agent name="quality-assurance">Review documentation for completeness, accuracy, and consistency</agent>
  <agent name="explore">Locate existing documentation patterns and API references to document</agent>
</related_agents>
