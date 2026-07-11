---
name: mdq
description: This skill should be used when the user asks to parse, search, grep, query, filter, or extract headings, sections, tasks, code blocks, links, or tables from Markdown files. Use when working with mdq, jq-style Markdown querying, section extraction, checklist validation, CI task scripts, or documentation automation pipelines.
version: 2.0.1
---

<purpose>
  Provide usage patterns for mdq, a CLI tool that queries Markdown documents using selector syntax mirroring Markdown itself.
</purpose>

<overview>
  mdq applies jq-style querying to Markdown files. Selectors mirror Markdown syntax, making queries intuitive. Results output as Markdown (default), JSON, or plain text.

  Basic usage: mdq [OPTIONS] [SELECTORS] [FILE...]
  Reads stdin if no file is given. Multiple files are concatenated.
</overview>

<selectors>
  <selector name="headers_sections">
    <syntax># title</syntax>
    <description>Select a section by heading. Matches the heading and all content until the next same-level heading.</description>
    <example>mdq '# installation' README.md</example>
    <example>mdq '# /getting.started/i' README.md</example>
  </selector>

  <selector name="unordered_list">
    <syntax>- item</syntax>
    <description>Select unordered list items matching text.</description>
    <example>mdq '- getting started' README.md</example>
    <example>mdq '-' README.md</example>
  </selector>

  <selector name="ordered_list">
    <syntax>1. item</syntax>
    <description>Select ordered list items matching text.</description>
    <example>mdq '1.' steps.md</example>
  </selector>

  <selector name="tasks">
    <syntax>- [ ] text  (unchecked)  /  - [x] text  (checked)  /  - [?] text  (any)</syntax>
    <description>Select task list items by completion state.</description>
    <example>mdq '- [ ]' tasks.md</example>
    <example>mdq '- [x]' tasks.md</example>
    <example>mdq '- [?]' tasks.md</example>
  </selector>

  <selector name="code_blocks">
    <syntax>```language text</syntax>
    <description>Select fenced code blocks, optionally filtering by language or content.</description>
    <example>mdq '```rust' guide.md</example>
    <example>mdq '```' README.md</example>
  </selector>

  <selector name="links">
    <syntax>[display](url)</syntax>
    <description>Select links by display text and/or URL pattern. Use * or empty to match any.</description>
    <example>mdq '[](github.com/)' docs.md</example>
    <example>mdq '[install](*)' README.md</example>
  </selector>

  <selector name="images">
    <syntax>![alt](url)</syntax>
    <description>Select images by alt text and/or URL.</description>
    <example>mdq '![](*)' README.md</example>
  </selector>

  <selector name="tables">
    <syntax>:-: column :-: row</syntax>
    <description>Select table rows. First :-: matches column header, second matches row content.</description>
    <example>mdq ':-: /Name/ :-: *' schedule.md</example>
    <example>mdq ':-: * :-: Alice' schedule.md</example>
  </selector>

  <selector name="blockquotes">
    <syntax>> text</syntax>
    <description>Select block quotes matching text.</description>
    <example>mdq '> note' docs.md</example>
  </selector>

  <selector name="paragraphs">
    <syntax>P: text</syntax>
    <description>Select paragraphs matching text.</description>
    <example>mdq 'P: /deprecated/' CHANGELOG.md</example>
  </selector>
</selectors>

<string_matching>
  <method name="unquoted">Case-insensitive, must start with a letter. Example: installation</method>
  <method name="quoted">Case-sensitive with escape sequences. Example: "Getting Started"</method>
  <method name="regex">/pattern/ using fancy-regex. Example: /getting.started/i</method>
  <method name="wildcard">* or empty matches anything.</method>
  <method name="anchors">^start or end$ for position anchoring.</method>
</string_matching>

<chaining>
  <description>Chain selectors with | to filter within results.</description>
  <example>
    # Extract unordered lists from the "Usage" section
    mdq '# usage | -' README.md

    # Get code blocks from the "Examples" section
    mdq '# examples | ```' guide.md

    # Find checked tasks in the "Release" section
    mdq '# release | - [x]' CHANGELOG.md
  </example>
</chaining>

<output_options>
  <option flag="--output markdown" short="-o markdown">Default. Outputs valid Markdown.</option>
  <option flag="--output json" short="-o json">JSON array for piping to jq or other tools.</option>
  <option flag="--output plain" short="-o plain">Plain text without Markdown formatting.</option>
  <option flag="--quiet" short="-q">Suppress stdout; exit code 0 if match found, 1 if not.</option>
  <option flag="--link-format inline">Inline links instead of reference-style.</option>
  <option flag="--link-format keep">Keep links in their original format (default is reference).</option>
</output_options>

<patterns>
  <pattern name="validate_checklist">
    <description>Check if all tasks are completed (exit code 0 = all done)</description>
    <example>
      # Fail if any unchecked tasks remain
      mdq -q '- [ ]' pull_request_template.md && echo "All done" || echo "Incomplete tasks"
    </example>
  </pattern>

  <pattern name="extract_links">
    <description>Extract all links as JSON for further processing</description>
    <example>
      mdq '[](*)' --output json docs.md | jq '.[].url'
    </example>
  </pattern>

  <pattern name="section_extraction">
    <description>Extract a specific section to a new file</description>
    <example>
      mdq '# api reference' README.md > api.md
    </example>
  </pattern>

  <pattern name="code_extraction">
    <description>Extract code examples by language</description>
    <example>
      mdq '```python' tutorial.md --output plain
    </example>
  </pattern>

  <pattern name="pipeline_with_jq">
    <description>Combine mdq and jq for complex document processing</description>
    <example>
      mdq '# changelog | - [x]' CHANGELOG.md --output json | jq 'length'
    </example>
  </pattern>
</patterns>

<decision_tree name="when_to_use_mdq">
  <question>Do you need to extract or query specific elements from a Markdown file?</question>
  <branch condition="Yes — section or heading">Use # selector, optionally chain with child selectors</branch>
  <branch condition="Yes — task completion">Use - [ ] / - [x] / - [?] selectors; use -q for scripting</branch>
  <branch condition="Yes — links or URLs">Use [](pattern) selector with --output json for structured data</branch>
  <branch condition="Yes — code blocks">Use ``` selector with optional language filter</branch>
  <branch condition="No — need full document processing">Use standard text tools (grep, awk) or a Markdown parser library</branch>
</decision_tree>

<best_practices>
  <practice priority="critical">Chain selectors with | to narrow scope before extracting child elements</practice>
  <practice priority="critical">Use --quiet (-q) and exit codes in CI/CD scripts instead of parsing stdout output</practice>
  <practice priority="high">Use --output json when piping results to jq or other structured-data tools</practice>
  <practice priority="high">Use regex selectors /pattern/i for case-insensitive matching in cross-platform environments</practice>
  <practice priority="medium">Prefer --output plain for text processing pipelines that do not need Markdown formatting</practice>
</best_practices>

<anti_patterns>
  <avoid name="non_markdown_files">
    <description>Applying mdq to non-CommonMark formats (AsciiDoc, RST, HTML) expecting Markdown-like results</description>
    <instead>Use mdq only on CommonMark Markdown files; use format-specific tools for other document types</instead>
  </avoid>

  <avoid name="stdout_parsing_in_ci">
    <description>Parsing mdq stdout in CI/CD to detect presence or absence of matches</description>
    <instead>Use --quiet (-q) flag and check exit code: 0 means match found, 1 means no match</instead>
  </avoid>

  <avoid name="unscoped_child_extraction">
    <description>Selecting deeply nested elements without first narrowing scope with a parent selector</description>
    <instead>Chain selectors (e.g., '# section | - [x]') to restrict extraction to the intended context</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Always chain selectors with | when extracting child elements to prevent scope bleed across sections</rule>
  <rule>Never parse mdq stdout in CI/CD; always use --quiet and check exit code (0 = match, 1 = no match)</rule>
  <rule>mdq only processes CommonMark Markdown; never apply it to AsciiDoc, RST, HTML, or other formats</rule>
</rules>

<rules priority="standard">
  <rule>Use --output json when piping to jq or other structured tools</rule>
  <rule>Match selectors to the Markdown element type, not text content alone</rule>
  <rule>Use regex (/pattern/i) for case-insensitive matching; use unquoted text only for simple case-insensitive single-word lookups</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Identify the target Markdown elements and their location</objective>
    <step order="1">Identify the element type (heading, list, task, code block, link, etc.)</step>
    <step order="2">Determine the required scope (full document or within a section)</step>
    <step order="3">Choose the output format (markdown, json, plain) based on downstream use</step>
  </phase>
  <phase name="implement">
    <objective>Construct and refine the mdq selector</objective>
    <step order="1">Write the primary selector matching the target element</step>
    <step order="2">Add pipe-chained parent selector if scope must be restricted</step>
    <step order="3">Add output flag if format other than Markdown is needed</step>
  </phase>
  <phase name="validate">
    <objective>Verify the output matches the expected content</objective>
    <step order="1">Run against a sample file and inspect output</step>
    <step order="2">Use --quiet and check the exit code (echo $status in fish, echo $? in bash) to confirm match behavior in scripts</step>
    <step order="3">Adjust selector if over-matched or under-matched</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">No matches found (exit code 1 in --quiet mode)</example>
    <example severity="medium">Unexpected output format or extra content extracted</example>
    <example severity="high">Wrong content extracted; selector matches unintended elements</example>
    <example severity="critical">mdq binary not found or not installed</example>
  </examples>
</error_escalation>

<related_agents>
  <agent name="explore">Locate Markdown files in the codebase before querying them</agent>
  <agent name="docs">Use extracted content to generate or update documentation</agent>
</related_agents>

<constraints>
  <must>Verify the target file is CommonMark Markdown before invoking mdq</must>
  <must>Use --quiet (-q) for boolean match tests in automated pipelines</must>
  <must>Keep selector guidance evidence-based: test selectors against sample files before recommending</must>
  <avoid>Applying mdq to non-CommonMark formats (AsciiDoc, RST, HTML)</avoid>
  <avoid>Parsing stdout to detect match presence; use exit codes instead</avoid>
  <avoid>Broad document-wide child selectors when a parent section is the actual intended scope</avoid>
</constraints>

<related_skills>
  <skill name="technical-documentation">General documentation authoring and structure</skill>
  <skill name="exploration-tools">Finding and querying files in a codebase</skill>
</related_skills>
