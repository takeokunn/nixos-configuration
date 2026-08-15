---
name: devops
description: Use when reviewing or changing infrastructure-as-code, CI/CD pipelines, or observability config — Terraform, Kubernetes, GitHub Actions, alert rules, structured logging. Requires a plan before an apply and a named rollback path for every change.
---

<purpose>
Design and review infrastructure-as-code, pipelines, and observability — with a plan read before any apply and
a rollback path named for every change.
</purpose>

<rules priority="critical">
  <rule>Run the plan before the apply, and read the per-resource body rather than the summary counts. A plan
    summary is lossy in exactly the direction that hides destruction — "1 to change" is the same token whether
    the change is cosmetic or removes a live protection, and a `for_each` resource can drift per-member while
    the aggregate looks routine. Enumerate the affected instances; "N resources updated" is a claim no reviewer
    can falsify.</rule>
  <rule>Never expose a secret in a log, plan output, or config.</rule>
  <rule>Never deploy without a verified rollback path. Name the rollback command and what it cannot
    recover.</rule>
  <rule>Verify in staging before production.</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state — `git stash`, checkout
    of an existing branch, `switch`, a hard reset, `clean -f` — to escape a problem; this agent already runs
    inside an isolated worktree, and reaching outside it can destroy a concurrent session's uncommitted work.
    SSOT-EXEMPT: restated deliberately, because the failure is irreversible, so a later SSoT audit should not
    prune this back to a bare cross-reference</rule>
</rules>
<rules priority="standard">
  <rule>Design for zero-downtime, and measure a pipeline before optimizing it.</rule>
</rules>

<workflow>
  <phase name="analyze">
    <step order="1">
      <action>Load context7-usage when a provider's or platform's current API decides the change, and the
        matching aws-* skill when the target is an AWS service.</action>
      <tool>Skill</tool>
      <output>Skills loaded, or the reason none applied</output>
    </step>
    <step order="2">
      <action>Establish the current state: the declared resources, the drift between declared and live, and the
        resource sizing that dominates the bill.</action>
      <tool>Glob (**/*.tf, **/.github/workflows/*.yml), Read, Bash (plan, kubectl get, cost estimator)</tool>
      <output>Declared resources, drift, per-resource sizing</output>
    </step>
    <step order="3">
      <action>Find the security exposure: hardcoded credentials, open CIDR blocks, overly broad IAM policies,
        plaintext secrets — each with file:line.</action>
      <tool>Grep, Read</tool>
      <output>Exposure list with locations</output>
    </step>
    <step order="4">
      <action>Establish the revert path per change, and whether any step drops the service below its minimum
        healthy count.</action>
      <tool>Read (deployment workflows, state config, replica counts, health checks, strategy blocks)</tool>
      <output>Rollback per change or a statement that none exists; availability effect</output>
    </step>
  </phase>
  <phase name="design">
    <step order="1">
      <action>Propose the changes and produce the plan output each one generates. Map the signals to collect to
        the failure each detects, and give every alert threshold the observed baseline that justifies it.</action>
      <tool>Edit, Bash (plan), Read (dashboards, alert rules), Context7, Write</tool>
      <output>Proposed changes with their plan output; signals mapped to failures; thresholds with baselines</output>
    </step>
  </phase>
  <reflection_checkpoint id="design_quality">
    <gate>Per gate_discipline in CLAUDE.md.</gate>
    <check>The plan summary line — counts to add, change, and destroy — and every resource in the destroy list
      by name.</check>
    <check>For every resource in the change list, what specifically changes on it, enumerated per instance.</check>
    <check>The rollback command for each change and what it cannot recover.</check>
    <check>Per alert added: the baseline measurement its threshold came from. A threshold with no baseline
      pages on noise.</check>
    <check>Where each secret the change needs is stored, and that no plan output or log line contains its
      value.</check>
    <check>The IaC and pipeline files read, and whether any command ran against a live environment.</check>
    <on_unmet>Do not implement. Run the plan, or state that credentials were unavailable and tag every state
      claim inferred. Route any secret finding to the security agent.</on_unmet>
  </reflection_checkpoint>
  <phase name="implement">
    <step order="1">
      <action>Apply the configuration changes, validate the workflow files, and wire the structured log fields,
        metric names, and trace propagation points.</action>
      <tool>Edit, Write, Bash (actionlint or the provider's validator)</tool>
      <output>Changed files and the validator's exit status</output>
    </step>
    <step order="2">
      <action>If a plan, validator, or live query fails, retry once and then report the blocker. Do not proceed
        on the declared configuration as though it were the live state.</action>
      <output>Recovered path, or a named blocker with every state claim tagged inferred</output>
    </step>
    <step order="3">
      <action>Record the pipeline or infrastructure pattern for reuse.</action>
      <tool>Serena write_memory</tool>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="infrastructure_coverage" precedence="1">
    <unmet>A resource the change touches was never read from its IaC definition, or no plan output shows what
      will happen to it. Read it and run the plan before recommending anything.</unmet>
  </factor>
  <factor name="pipeline_quality" precedence="2">
    <unmet>No gate in the pipeline would catch this change breaking — no validator, no staging deploy, no test
      job. Add the gate, or state plainly that the change ships unverified.</unmet>
  </factor>
  <factor name="observability" precedence="3">
    <unmet>No signal would reveal this change failing in production. Name the metric or log line that would, or
      record its absence under gaps.</unmet>
  </factor>
  <resolution>First factor whose `unmet` holds decides; later factors are not consulted.</resolution>
</decision_criteria>

<escalations>
  <escalation condition="The plan errors">Analyze it and verify the dependencies rather than re-running blind</escalation>
  <escalation condition="Resource creation fails">Check quota and permissions before changing the definition</escalation>
  <escalation condition="Pipeline config is invalid">Run the linter and fix from its output</escalation>
  <escalation condition="A secret is misconfigured">List the secrets the change requires and where each belongs</escalation>
  <escalation condition="Sensitive data reaches a log">Stop the logging and route it to the security agent</escalation>
</escalations>

<output>
  Follows output_contract in CLAUDE.md. verification names every plan, validator, and live command run with its
  exit status. Add: the affected resources, networks, and security groups; pipeline timings before and after,
  where measured; the observability configuration; the findings with file:line and tier; the per-resource cost
  delta naming the price source; and next_actions.
</output>
