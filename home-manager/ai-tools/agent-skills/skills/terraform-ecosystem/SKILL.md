---
name: terraform-ecosystem
description: Use for Terraform or OpenTofu HCL configuration and provider development (Go, terraform-plugin-framework). Covers state management, plan and apply, failed-apply recovery, moved blocks, import, and provider schema design.
version: 3.0.0
---

Two pillars: authoring custom providers with terraform-plugin-framework in Go, and writing/operating HCL
(lifecycle management, credential scoping, DNS+hosting composition, CI plan/apply chains, state management).
Concrete providers below are illustrative only — the mechanisms generalize. For Go idioms (error handling,
module layout, table-driven tests) and for dev-shell setup, see the Related links at the end; this skill
covers only the Terraform-specific surface on top of those.

## Provider development (terraform-plugin-framework)

Verify exact symbol names with Context7 (`/hashicorp/terraform-plugin-framework`) before relying on them —
helper package names differ from the pre-1.0 design docs. Target new provider work at
terraform-plugin-framework, not SDKv2 (`helper/schema`, `CreateContext` with `*schema.ResourceData`); do not
mix the two idioms in one resource.

### Provider and resource interfaces

The provider implements `provider.Provider`: `Metadata` (type name prefix + version), `Schema`
(provider-level config), `Configure` (builds the API client, passes it via `resp.ResourceData` /
`resp.DataSourceData`), `Resources`/`DataSources` (constructor lists). Interface satisfaction is
compile-checked with a blank var assignment.

A resource implements the base `resource.Resource` (Metadata, Schema, Create, Read, Update, Delete) plus
optional extension interfaces — `resource.ResourceWithConfigure` (adds `Configure`) and
`resource.ResourceWithImportState` (adds `ImportState`, required for `terraform import` support). These are
kept as separate interfaces deliberately: the framework only invokes `Configure`/`ImportState` when the
resource actually satisfies the corresponding extension. Treating them as base-interface methods is the
common mistake.

```go
type ExampleProvider struct{ version string }

var _ provider.Provider = &ExampleProvider{}

func (p *ExampleProvider) Metadata(ctx context.Context, req provider.MetadataRequest, resp *provider.MetadataResponse) {
    resp.TypeName = "example"
    resp.Version = p.version
}

var (
    _ resource.Resource                = &CacheResource{}
    _ resource.ResourceWithConfigure   = &CacheResource{}
    _ resource.ResourceWithImportState = &CacheResource{}
)
```

### Schema, plan modifiers, validators

Required/Optional/Computed drives planning: Computed means the provider supplies the value, possibly unknown
at plan time; Optional+Computed means the user may set it but the provider fills a default when unset;
Sensitive redacts it from CLI output and logs. Declare these deliberately — they are read by Terraform's
planner, not just documentation.

```go
"name": schema.StringAttribute{
    Required: true,
    PlanModifiers: []planmodifier.String{stringplanmodifier.RequiresReplace()},
    Validators: []validator.String{
        stringvalidator.LengthBetween(1, 63),
        stringvalidator.RegexMatches(regexp.MustCompile(`^[a-z0-9-]+$`), "must be lowercase, digits, hyphens"),
    },
},
"uri": schema.StringAttribute{
    Computed:      true,
    PlanModifiers: []planmodifier.String{stringplanmodifier.UseStateForUnknown()},
},
```

Plan modifiers adjust the planned value before apply: `UseStateForUnknown` stops a stable Computed attribute
from showing `(known after apply)` on every plan; `RequiresReplace` forces destroy-and-recreate for an
attribute the backing API cannot update in place; `RequiresReplaceIf` conditions replacement on old/new
values when only some changes are destructive. Pick `UseStateForUnknown` for Computed-and-stable,
`RequiresReplace` for immutable, `RequiresReplaceIf` for conditionally destructive, nothing for ordinary
mutable attributes.

Validators (`stringvalidator.LengthBetween`, `RegexMatches`, `OneOf`, `int64validator.Between`, from
terraform-plugin-framework-validators, not hand-rolled) reject bad config at plan time with a clear
diagnostic and no API call. Reserve round-trips to the API for CRUD.

### CRUD — the silent failure mode is in Read

`Create`: read the plan into a model (`req.Plan.Get`), call the API, and — since a create response is
commonly empty — follow with a GET to hydrate Computed attributes before `resp.State.Set`. Do not assume
the create response carries every attribute.

`Read` is where drift detection and the critical 404 handling live. **When the backing object is gone
(HTTP 404), remove it from state and return with no error** — adding an error here wedges the user, because
there is now no way to reconcile short of manual state surgery:

```go
obj, err := r.client.GetCache(ctx, state.Name.ValueString())
if err != nil {
    if errors.Is(err, ErrNotFound) {
        resp.State.RemoveResource(ctx)
        return
    }
    resp.Diagnostics.AddError("Read failed", err.Error())
    return
}
```

`Update` never receives an attribute marked `RequiresReplace` — the framework schedules a replace instead.
`Delete` should treat 404 as success (already gone); on success the framework calls
`State.RemoveResource` automatically.

`Configure` on a resource must nil-check `req.ProviderData` (the framework calls it with nil during earlier
lifecycle phases) and type-assert defensively before use. Identity/account context that many endpoints need
(an account ID resolved via a "current user" lookup) is best resolved once in the provider's `Configure` and
carried on the client, not re-fetched per CRUD call.

`ImportState` seeds enough state from the import ID for the subsequent `Read` to hydrate the rest; the
simplest form passes the ID straight into an identifying attribute via
`resource.ImportStatePassthroughID(ctx, path.Root("name"), req, resp)`.

### HTTP client and acceptance tests

Keep retry/error-classification in the client so CRUD stays declarative: retry 5xx and 429 (honor
`Retry-After`, exponential backoff base\*2^attempt capped, respecting context cancellation on every
attempt); do not retry other 4xx. 401/403 usually indicates a credential-scope problem (see
credential-scope troubleshooting below), not a config bug; 404 maps to remove-from-state in Read and
success in Delete.

Acceptance tests are gated behind `TF_ACC` so they never run during ordinary unit testing — they create and
destroy real resources. Wire the in-process provider with `ProtoV6ProviderFactories` +
`providerserver.NewProtocol6WithError` (protocol 6, the framework default; `ProtoV5ProviderFactories` for
protocol 5 or muxed setups). `PreCheck` validates required credentials are present before the test runs.

```go
func TestAccCacheResource_Basic(t *testing.T) {
    resource.Test(t, resource.TestCase{
        PreCheck:                 func() { testAccPreCheck(t) },
        ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
        Steps: []resource.TestStep{
            {Config: testAccCacheConfig("example-cache"), Check: resource.ComposeTestCheckFunc(
                resource.TestCheckResourceAttr("example_cache.test", "name", "example-cache"),
                resource.TestCheckResourceAttrSet("example_cache.test", "uri"),
            )},
            {ResourceName: "example_cache.test", ImportState: true, ImportStateVerify: true},
        },
    })
}
```

Run with `TF_ACC=1 go test -v ./...`.

## HCL authoring and operations

### `ignore_changes`, scoped narrowly

`lifecycle.ignore_changes` stops Terraform reconciling specific attributes after create, for fields set once
but subsequently managed elsewhere (a CI pipeline, another controller, the platform itself). Scope it to the
narrowest attribute path — `ignore_changes = [deployment[0].source]`, not the whole block. Ignoring too much
hides real drift; it is an escape hatch, not a default.

### Credential-scope failures look like configuration bugs

A frequent, confusing failure: some resources apply cleanly while others fail at apply with 404 or 403 on
create, under the same run. **When one resource type works and another does not, suspect the execution
credential's scope before the configuration** — a 404 on create is a common signal that the credential
cannot see the endpoint at all, not that the resource block is wrong. Diagnose by comparing the failing
endpoint's privilege tier against the working ones, then confirm against the token's granted scopes — not by
rewriting the resource block. If the failing endpoint's capability isn't needed, removing that resource from
config is safe only after confirming with `terraform state list` that it holds no state entry (so apply
completes without a destroy).

### `provider =` cannot transfer ownership — this is the sharpest trap in the pillar

A workspace often manages resources under two owners (accounts, orgs, regions, credentials) via a second
`provider` block with an `alias`, plus `provider = example.secondary` on every resource belonging to it. Put
the `provider =` line first in the resource body and split resource files by owner, so ownership is never
inferred from a credential variable name several files away.

```hcl
provider "example" { token = var.primary_token }               # default owner
provider "example" { alias = "secondary"; owner = var.secondary_owner; token = var.secondary_token }

resource "example_thing" "shared" {
  provider = example.secondary
  name     = "thing"
}
```

**No Terraform resource performs an ownership transfer of the underlying object.** Changing a resource's
`provider =` only changes which credential Terraform uses to look for it — the object's identity (which
usually embeds an owner/account/zone/project) does not move with an address rewrite. On the next refresh
Terraform finds nothing under the new owner, concludes the object is gone (silently orphaning the real one,
which still holds its data), and plans a brand-new empty resource under the new owner. Observed signature: a
same-address owner change plans as `N to add, 0 to change, M to destroy` — never as an in-place change. Any
resource whose identity includes an owner/account/org/project/zone segment behaves this way when that
segment moves; "the plan says destroy and create but I only changed which provider manages it" is a
data-loss signal, never a rename.

Safe sequence: (1) transfer the real object out-of-band via whatever mechanism the platform itself provides
— Terraform cannot do this step; (2) `terraform state rm` the address so the stale entry stops driving a
destroy; (3) re-import under the aliased provider — `terraform import -provider=example.secondary <address>
<id>`, or an `import {}` block carrying `provider = example.secondary` when the CLI form is unavailable (see
below); (4) require a zero-diff plan before applying anything else — a non-empty plan means the import did
not match the real object.

### Declarative `import {}` vs the `terraform import` CLI

These are not stylistic alternatives — the difference that matters is *where the provider gets configured*.
Some subcommands run where you type them; others run in the backend. `terraform import` executes **locally**.
Against a remote backend holding credentials as server-side sensitive workspace variables, the local run has
no values for them, so provider configuration cannot complete. The failure reads like an unresolvable
expression in a provider block — "cannot be determined until apply" — and points at the configuration, but
the configuration is fine; the credentials simply are not present on the machine running the command.

Use a declarative import block instead — it is processed during an ordinary `plan`/`apply`, which runs
remotely and has credential access:

```hcl
import {
  to = example_thing.shared
  id = "thing-identifier"
  # provider = example.secondary   # when importing under an aliased provider
}
```

Review the resulting plan (should report imported with no changes), apply, then delete the import block.
Generalize the diagnosis, not just the fix: the same locality argument applies to any local-only subcommand
run against a remote-backend workspace — when an error blames configuration you can see is valid, ask which
side of the backend boundary the operation executed on.

### A failed `apply` does not roll back

Everything that completed before the failure stays created and stays in state; the run simply stops where it
broke. Resources created earlier in the run remain and remain tracked — re-running apply will not recreate
them. Resources that failed to *destroy* stay correctly tracked, so state is not corrupted, just incomplete
relative to intent. **"The apply failed, so nothing happened" is the most expensive wrong assumption
available in Terraform operations** — the correct default is that an unknown prefix of the change is now
live.

Recovery: read the actual state (`terraform state list`, `terraform show`) before re-running anything — do
not reason from the configuration you intended to apply. Where an object must go but the provider couldn't
destroy it, delete it out-of-band and reconcile the dangling entry with `terraform state rm`. Require a clean
plan matching your intent before the next apply.

### Plan summary counts hide what changed

`Plan: N to add, M to change, K to destroy` counts resource addresses; it does not describe *what* changed. A
single "to change" can be an innocuous edit or the removal of a live protection, indistinguishable in the
summary — most dangerous on a shared policy resource driven by `for_each`, where drift on one member appears
as one routine-looking change among many. Protections added out-of-band through a console appear as `-`
lines removing them, because the config never knew they existed; applying without reading the full plan body
strips them silently and reports success. Read every `-` and `~` line before approving an apply that touches
a policy, ruleset, ACL, or permissions resource.

When one `for_each` member has legitimately diverged (console-added protections the shared shape can't
express), absorb it without ever leaving the object unprotected: copy the live values out of the plan's `-`
lines (authoritative record of what exists now); write a dedicated resource for that member with the merged
config; remove that key from the shared `for_each` map; add a `moved` block from the old instance address to
the new one; confirm the plan shows the instance moved with zero changes before applying.

```hcl
moved {
  from = example_policy.shared["member-key"]
  to   = example_policy.member_key
}
```

Without the `moved` block this refactor is destroy-then-create, and the policy is genuinely absent for the
duration of the apply — a real protection gap on a real resource. With a matched `moved` block and identical
rule content, the plan shows 0 changes and there is no window without protection (verified empirically, not
inferred). `moved` blocks are the general mechanism for any resource rename or module restructure; a rename
without one is always destroy-and-create regardless of how the diff looks at a glance.

### DNS + hosting composition

A recurring shape: exposing a service under a custom domain needs two coordinated resources across two
providers — the hosting/platform resource that claims the domain (a CNAME/custom-domain field, triggering
certificate provisioning) and a DNS record routing to the platform's target. One without the other yields a
broken or unverified domain.

```hcl
resource "hosting_site" "example" {
  name = "example"
  deployment { build_type = "workflow"; domain = "sub.example.com" }
}

resource "dns_record" "example_sub" {
  zone_id = dns_zone.example.id
  name    = "sub"
  type    = "CNAME"
  content = "example.hosting-platform.net"  # platform's canonical host
  ttl     = 1                                # 1 = automatic
  proxied = false                            # direct CNAME, not proxied
}
```

Keep the DNS record unproxied when the platform terminates TLS and validates the domain via direct
CNAME — proxying can break domain verification. Resource names above are placeholders; the two-resource
composition is the reusable idea, applicable to any static-hosting-plus-DNS pairing.

### State isolation and CI

Isolate independent concerns into separate workspaces/root modules (DNS, source-control, compute each with
their own state) so a plan/apply in one never evaluates or risks another. Use a remote backend as the single
source of truth; avoid local state for shared infrastructure. Keep secrets out of state and config — a
secrets manager or encrypted file (SOPS), never plaintext `.tf`, and mark token attributes `Sensitive`.

CI chain, cheapest checks first: `terraform fmt -check` → `terraform validate` (no remote calls) → `tflint`
→ `terraform plan` (the reviewable artifact, per isolated project) → `terraform apply` (gated behind
review, scoped to the changed project). Pin Terraform/tofu, tflint, and provider versions declaratively (a
Nix/devenv shell entered via `... --command` is one way) so local and CI runs use identical binaries.

## Context7 lookups

The framework's public helper packages (`stringplanmodifier`, `stringvalidator`, `providerserver`) differ
from older pre-1.0 design docs — verify before quoting exact names. Libraries:
`/hashicorp/terraform-plugin-framework`, `/hashicorp/terraform-plugin-testing`,
`/websites/developer_hashicorp_terraform`. Useful lookup topics: plan-modifier helper signatures,
validators-package APIs, `ProtoV5`/`ProtoV6ProviderFactories` and `providerserver` factory helpers,
`ignore_changes`/`replace_triggered_by`/`precondition` semantics.

## Related

- [serena-usage](../serena-usage/SKILL.md) — navigate provider Go symbols and HCL references efficiently.
- [context7-usage](../context7-usage/SKILL.md) — fetch current terraform-plugin-framework and Terraform
  documentation.
- [investigation-patterns](../investigation-patterns/SKILL.md) — evidence-based diagnosis of plan diffs and
  apply-time failures.
