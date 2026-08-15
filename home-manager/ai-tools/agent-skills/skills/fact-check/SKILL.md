---
name: Fact Check
description: This skill should be used when the user asks to "verify claims", "fact check", "validate documentation", "check sources", or needs verification of external source references. Provides patterns for systematic fact verification using Context7 and WebSearch.
version: 2.1.0
---

<purpose>
  Provide patterns and methodology for systematic fact-checking of claims against authoritative external sources. This skill contains the knowledge (patterns, concepts, best practices) for verification operations.
</purpose>

<tools>
  <tool name="resolve-library-id">
    <description>Resolve package name to Context7-compatible library ID</description>
    <param name="libraryName">Library name to search for</param>
    <use_case>Must call before query-docs for library documentation claims</use_case>
  </tool>

  <tool name="query-docs">
    <description>Fetch documentation for a specific library to verify claims</description>
    <param name="libraryId">Library ID from resolve-library-id</param>
    <param name="query">Specific topic or question to verify, scoped to a single concept</param>
    <use_case>Verify claims about library APIs, behavior, and best practices</use_case>
  </tool>

  <tool name="WebSearch">
    <description>Search web for verification of general claims</description>
    <param name="query">Search query for verification</param>
    <use_case>Verify claims about standards, specifications, and general technical facts</use_case>
  </tool>

  <tool name="WebFetch">
    <description>Fetch specific URL content for verification</description>
    <param name="url">URL to fetch</param>
    <param name="prompt">Extraction prompt for relevant content</param>
    <use_case>Verify claims against specific documentation pages or specifications</use_case>
  </tool>
</tools>

<patterns>
  <pattern name="claim_extraction">
    <description>Identify claims that reference external sources for verification</description>
    <decision_tree name="when_to_use">
      <question>Does the content reference external documentation or standards?</question>
      <if_yes>Apply claim extraction to identify verifiable assertions</if_yes>
      <if_no>No fact-checking needed for this content</if_no>
    </decision_tree>
    <example>
      Claim types to extract:
      Library API claims: "useState returns a tuple"
      Documentation references: "according to the React docs"
      Standard compliance: "follows WCAG 2.1 AA"
      Version-specific behavior: "in React 18, Suspense..."
      Performance claims: "O(log n) complexity per MDN"

      Version-specific example:
      Claim: "React 18 introduces automatic batching for all updates"
      Verification: Query Context7 with query="batching" for React 18 docs
      Result: Confirmed - React 18 automatically batches state updates inside promises, setTimeout, and native event handlers
    </example>
  </pattern>

  <pattern name="source_selection">
    <description>Choose appropriate verification source based on claim type</description>
    <decision_tree name="when_to_use">
      <question>What type of claim needs verification?</question>
      <branch condition="Library/framework API">Use Context7 with resolve-library-id then query-docs</branch>
      <branch condition="Web standard/specification">Use WebSearch for official specification</branch>
      <branch condition="General technical fact">Use WebSearch with authoritative domain filter</branch>
      <branch condition="Specific documentation URL">Use WebFetch to retrieve and verify</branch>
    </decision_tree>
    <example>
      Source priority:
      Context7 for library documentation (prefer High Source Reputation and higher Benchmark Score)
      WebFetch for specific URLs cited in claims
      WebSearch for general technical claims
      Mark as unverifiable if no source available
    </example>
  </pattern>

  <pattern name="evidence_tier_assignment">
    <description>Classify how a claim's verification came to be known, rather than scoring it. A tier
      is checkable — a reader can re-run the same source query and see whether it holds. A confidence
      number produced in the same pass that did the verification never contradicts that verification,
      so nothing downstream ever reads a low score and stops trusting the claim (CLAUDE.md
      evidence_and_reporting, core-patterns evidence_tiers, CORE-P001).</description>
    <decision_tree name="when_to_use">
      <question>Has verification evidence been collected?</question>
      <if_yes>Apply evidence tier assignment to classify how the claim is now known</if_yes>
      <if_no>Continue evidence collection before assessment</if_no>
    </decision_tree>
    <example>
      verified: The source was queried (Context7, WebFetch, or WebSearch against an authoritative
        domain) and its content directly supports the claim as stated. Cite the source and the
        matching text.
      inferred: The source was queried and supports a related claim, but the claim under review adds a
        step the source did not state directly. Name that step so it can be disputed.
      assumed: No source could be located or queried for this claim. State what would confirm it —
        which library, which spec, which command — rather than marking it verified.

      Disposition: a claim resting on `assumed` evidence is unverified, not confirmed, until a source
      is actually queried.
    </example>
  </pattern>

  <pattern name="discrepancy_reporting">
    <description>Format and report verification failures with evidence</description>
    <decision_tree name="when_to_use">
      <question>Does the claim's evidence tier come out as inferred, assumed, or disputed rather than
        verified?</question>
      <if_yes>Apply discrepancy reporting to document the issue</if_yes>
      <if_no>Mark claim as verified</if_no>
    </decision_tree>
    <example>
      Discrepancy report format:
      Claim: Original assertion made
      Source: Where claim was made
      Verification source: Context7/WebSearch result
      Evidence: Actual information from source
      Evidence tier: verified / inferred / assumed / disputed, with what would raise it
      Recommendation: Suggested correction or note
    </example>
  </pattern>
</patterns>

<concepts>
  <concept name="verification_sources">
    <description>Authoritative sources for different claim types</description>
    <example>
      Library documentation: Context7 MCP
      React: /facebook/react
      Next.js: /vercel/next.js
      TypeScript: /microsoft/typescript
      NixOS: /nixos/nixpkgs

      Web standards: WebSearch with domain filters
      MDN Web Docs: developer.mozilla.org
      W3C: w3.org
      WHATWG: html.spec.whatwg.org
      OWASP: owasp.org
    </example>
  </concept>

  <concept name="claim_types">
    <description>Categories of verifiable claims</description>
    <example>
      API behavior: Function signatures, return types, parameters
      Configuration: Config options, default values, valid settings
      Best practices: Recommended patterns from official docs
      Deprecation: API deprecation status and alternatives
      Compatibility: Version compatibility and requirements
      Performance: Complexity claims, benchmark references
      Security: Security recommendations and vulnerability info
    </example>
  </concept>

  <concept name="evidence_tier_disposition">
    <description>What each evidence tier means for a fact-check result, and what happens to the claim
      next — the concrete disposition a tier implies, not a number to interpret</description>
    <example>
      verified: Claim matches the queried source. Report it as confirmed and cite the source.
      inferred: The source supports a related claim; the claim under review extends it by a step that
        was never directly observed. Report it with that step named, so a reader can dispute the step
        rather than the source.
      assumed: No source was queried, or none was available. Report the claim as unverifiable, state
        what would confirm it, and do not present it as fact-checked.
      disputed: A queried source directly contradicts the claim. This is a distinct outcome from
        `assumed` — the source was checked and it disagrees, which is worse news than absence and must
        be surfaced as a contradiction, not filed alongside the merely-unchecked claims.
    </example>
  </concept>
</concepts>

<best_practices>
  <practice priority="critical">Use Context7 as primary source for library documentation claims</practice>
  <practice priority="critical">Flag every claim whose evidence tier is inferred, assumed, or disputed; a tier of verified needs no flag</practice>
  <practice priority="critical">Document evidence source for each verification</practice>
  <practice priority="high">Prefer libraries with High Source Reputation and a strong Benchmark Score in Context7 for verification</practice>
  <practice priority="high">Use WebSearch fallback when Context7 unavailable</practice>
  <practice priority="medium">Include direct quotes from sources as evidence</practice>
  <practice priority="medium">Note when verification source has version mismatch</practice>
</best_practices>

<anti_patterns>
  <avoid name="assumption_verification">
    <description>Marking claims as verified without actual source check</description>
    <instead>Always query Context7 or WebSearch for evidence before marking verified</instead>
  </avoid>

  <avoid name="single_source_reliance">
    <description>Relying on only one source for disputed claims</description>
    <instead>Cross-reference with multiple sources when the evidence tier is inferred rather than verified</instead>
  </avoid>

  <avoid name="ignoring_version_context">
    <description>Verifying claims without considering version differences</description>
    <instead>Note version context and verify against appropriate documentation version</instead>
  </avoid>

  <avoid name="over_verification">
    <description>Attempting to verify every statement including obvious facts</description>
    <instead>Focus on claims referencing external sources, APIs, and specifications</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Always verify claims against authoritative sources before flagging</rule>
  <rule>Use Context7 as primary source for library and framework claims</rule>
  <rule>Flag claims whose evidence tier is inferred, assumed, or disputed in fact check results</rule>
  <rule>Document evidence source for every verification</rule>
</rules>

<rules priority="standard">
  <rule>Use WebSearch as fallback when Context7 unavailable</rule>
  <rule>Prefer official documentation over third-party sources</rule>
  <rule>Note version context when verifying version-specific claims</rule>
  <rule>Cross-reference disputed claims with multiple sources</rule>
</rules>

<error_escalation>
  <examples>
    <example severity="low">Claim cannot be verified due to missing documentation</example>
    <example severity="medium">Conflicting information from different sources</example>
    <example severity="high">Claim directly contradicts authoritative source</example>
    <example severity="critical">Security-related claim is incorrect</example>
  </examples>
</error_escalation>

<constraints>
  <must>Query authoritative sources before verification</must>
  <must>Document evidence for all verification results</must>
  <must>Flag discrepancies with their evidence tier (verified/inferred/assumed/disputed), never a numeric confidence score</must>
  <avoid>Marking claims verified without source check</avoid>
  <avoid>Verifying claims based on assumption or memory</avoid>
  <avoid>Ignoring version context in verification</avoid>
</constraints>

<related_skills>
  <skill name="context7-usage">Core tool for library documentation verification</skill>
  <skill name="investigation-patterns">Evidence collection methodology</skill>
  <skill name="technical-documentation">Documentation accuracy standards</skill>
</related_skills>

<related_agents>
  <agent name="explore">Locate referenced code patterns and claims in the codebase</agent>
  <agent name="quality-assurance">Verify factual accuracy of implementation assertions</agent>
</related_agents>
