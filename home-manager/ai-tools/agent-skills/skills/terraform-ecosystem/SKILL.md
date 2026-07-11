---
name: terraform-ecosystem
description: This skill should be used when the user asks to "write terraform", "HCL", "terraform provider development", "terraform-plugin-framework", "custom provider", "state management", "terraform plan", "terraform apply", "lifecycle ignore_changes", "provider schema", "plan modifier", "acceptance test", or works with Terraform/OpenTofu configuration and provider authoring. Provides patterns for both custom provider development (Go, terraform-plugin-framework) and HCL configuration/operations.
version: 2.0.0
---

<purpose>
  Provide comprehensive patterns for the Terraform ecosystem across two pillars: (1) authoring custom providers with terraform-plugin-framework in Go, and (2) writing and operating HCL configurations (lifecycle management, credential scoping, DNS + hosting composition, CI plan/apply chains, state management). Emphasizes generalized principles over any single service, with concrete providers used only as illustrative examples.
</purpose>

<scope>
  <focus>terraform-plugin-framework provider authoring and HCL configuration/operations patterns</focus>
  <defer_to skill="golang-ecosystem">
    Go language syntax, error handling idioms, module layout, table-driven tests, goroutines/context. Provider code is Go; this skill covers only the Terraform-specific surface on top of Go.
  </defer_to>
  <defer_to skill="devenv-ecosystem">
    Development shell setup, languages.terraform/opentofu, git-hooks (tflint, terraform fmt) for Terraform projects.
  </defer_to>
  <unique_coverage>
    Provider/Resource/DataSource interfaces, Schema and attribute definitions, plan modifiers, validators, CRUD lifecycle with 404 state removal, Configure/client injection, ImportState, HTTP retry/error classification for API-backed providers, acceptance testing (TF_ACC, protocol factories); HCL lifecycle meta-arguments, credential-scope troubleshooting, DNS + hosting two-resource composition, per-project state isolation, plan/apply validation chains.
  </unique_coverage>
</scope>

<tools>
  <tool name="terraform init">Initialize provider plugins and backend (run per project/workspace)</tool>
  <tool name="terraform plan">Preview changes; the primary review artifact in CI</tool>
  <tool name="terraform apply">Apply changes to real infrastructure</tool>
  <tool name="terraform fmt">Canonicalize HCL formatting</tool>
  <tool name="terraform validate">Validate configuration internally (no remote calls)</tool>
  <tool name="terraform import">Bring existing infrastructure under management</tool>
  <tool name="tflint">Lint HCL for provider-specific and generic issues</tool>
  <tool name="go test">Run provider unit and acceptance tests (acceptance requires TF_ACC=1)</tool>
  <tool name="Read">Analyze .tf files and Go provider source</tool>
  <tool name="Edit">Modify HCL and provider code</tool>
  <tool name="mcp__plugin_claude-code-home-manager_context7__query-docs">Fetch current terraform-plugin-framework and Terraform documentation</tool>
</tools>

<concepts>
  <concept name="plugin_framework_vs_sdkv2">terraform-plugin-framework is the current, recommended way to build providers; it is interface-based (Provider/Resource/DataSource with typed request/response structs). SDKv2 (helper/schema, CreateContext with *schema.ResourceData) is legacy. Do not mix their idioms in one resource; new work should target the framework.</concept>
  <concept name="desired_state_reconciliation">Terraform reconciles configuration (desired) against state (last known) against the real world (Read). Providers must make Read authoritative so plans are accurate and drift is detected.</concept>
  <concept name="computed_vs_optional">Attributes are Required, Optional, and/or Computed. Computed values are supplied by the provider (unknown at plan time until stabilized). Correct plan behavior depends on marking these accurately.</concept>
  <concept name="credential_scope">The credential Terraform executes with defines what it can do. A config that is syntactically correct can still fail at apply because the token lacks scope for a specific endpoint. Diagnose by endpoint, not by config.</concept>
</concepts>

<!-- ============================================================= -->
<!-- PILLAR 1: PROVIDER DEVELOPMENT (terraform-plugin-framework)    -->
<!-- ============================================================= -->

<provider_development>
  <description>Authoring a custom provider with terraform-plugin-framework. Provider code is Go; see golang-ecosystem for language idioms. Verify exact symbol names with Context7 (/hashicorp/terraform-plugin-framework) before relying on them, as helper package names differ from the pre-1.0 design docs.</description>

  <provider_interface>
    <description>The provider is the top-level object. It implements provider.Provider and declares the resources and data sources it offers. Interface satisfaction is compile-checked with a blank var assignment.</description>
    <required_methods>
      <method name="Metadata">Sets the provider type name (the prefix for resource type names) and version.</method>
      <method name="Schema">Declares provider-level configuration (endpoints, tokens).</method>
      <method name="Configure">Reads provider config, constructs the API client, and passes it to resources/data sources via resp.ResourceData and resp.DataSourceData.</method>
      <method name="Resources">Returns the list of resource constructors.</method>
      <method name="DataSources">Returns the list of data source constructors.</method>
    </required_methods>
    <example>
      type ExampleProvider struct {
          version string
      }

      var _ provider.Provider = &amp;ExampleProvider{}

      func (p *ExampleProvider) Metadata(ctx context.Context, req provider.MetadataRequest, resp *provider.MetadataResponse) {
          resp.TypeName = "example"
          resp.Version = p.version
      }

      func (p *ExampleProvider) Resources(ctx context.Context) []func() resource.Resource {
          return []func() resource.Resource{
              NewCacheResource,
          }
      }
    </example>
  </provider_interface>

  <resource_interfaces>
    <description>A resource implements resource.Resource (base) plus optional extension interfaces for capabilities. Keeping these as separate interfaces is intentional: the framework only invokes Configure/ImportState when the corresponding interface is satisfied.</description>
    <interface name="resource.Resource">
      <requires>Metadata, Schema, Create, Read, Update, Delete</requires>
      <description>The base interface. Every resource must implement the full CRUD set plus schema and metadata.</description>
    </interface>
    <interface name="resource.ResourceWithConfigure">
      <requires>Configure</requires>
      <description>Extension that receives the shared API client from the provider's Configure. Guard against a nil ProviderData (the framework calls Configure with nil during earlier lifecycle phases).</description>
    </interface>
    <interface name="resource.ResourceWithImportState">
      <requires>ImportState</requires>
      <description>Extension that enables `terraform import`. Without it, import is unsupported.</description>
    </interface>
    <example>
      type CacheResource struct {
          client *APIClient
      }

      var (
          _ resource.Resource                = &amp;CacheResource{}
          _ resource.ResourceWithConfigure   = &amp;CacheResource{}
          _ resource.ResourceWithImportState = &amp;CacheResource{}
      )
    </example>
    <common_mistake>
      Treating Configure and ImportState as if they were methods of resource.Resource. They belong to the ResourceWithConfigure and ResourceWithImportState extension interfaces respectively; the base Resource interface is only Metadata/Schema/CRUD.
    </common_mistake>
  </resource_interfaces>

  <schema_and_attributes>
    <description>Schema declares attribute types and behavior. The Required/Optional/Computed triad drives planning; declare it deliberately.</description>
    <attribute_flags>
      <flag name="Required">Must be set by the user.</flag>
      <flag name="Optional">May be set by the user.</flag>
      <flag name="Computed">Set by the provider; may be unknown at plan time.</flag>
      <flag name="Optional+Computed">User may set it; if unset, the provider supplies a value (default-like behavior).</flag>
      <flag name="Sensitive">Redacted in CLI output and logs (tokens, keys).</flag>
    </attribute_flags>
    <example>
      func (r *CacheResource) Schema(ctx context.Context, req resource.SchemaRequest, resp *resource.SchemaResponse) {
          resp.Schema = schema.Schema{
              Attributes: map[string]schema.Attribute{
                  "name": schema.StringAttribute{
                      Required: true,
                      PlanModifiers: []planmodifier.String{
                          stringplanmodifier.RequiresReplace(),
                      },
                      Validators: []validator.String{
                          stringvalidator.LengthBetween(1, 63),
                          stringvalidator.RegexMatches(
                              regexp.MustCompile(`^[a-z0-9-]+$`),
                              "must contain only lowercase letters, digits, and hyphens",
                          ),
                      },
                  },
                  "uri": schema.StringAttribute{
                      Computed: true,
                      PlanModifiers: []planmodifier.String{
                          stringplanmodifier.UseStateForUnknown(),
                      },
                  },
                  "is_public": schema.BoolAttribute{
                      Optional: true,
                      Computed: true,
                  },
              },
          }
      }
    </example>
  </schema_and_attributes>

  <plan_modifiers>
    <description>Plan modifiers adjust the planned value of an attribute before apply. The framework ships typed helper packages (stringplanmodifier, boolplanmodifier, int64planmodifier, etc.). Custom modifiers implement the planmodifier interface for the type.</description>
    <modifier name="UseStateForUnknown">
      <use_case>Computed attribute that does not change once set. Prevents a Computed value from showing "(known after apply)" on every plan when it is actually stable, reducing plan noise.</use_case>
      <example>stringplanmodifier.UseStateForUnknown()</example>
    </modifier>
    <modifier name="RequiresReplace">
      <use_case>Immutable attribute. A change forces destroy-and-recreate instead of an in-place update (e.g. a resource whose backing API has no rename/update endpoint).</use_case>
      <example>stringplanmodifier.RequiresReplace()  // also boolplanmodifier.RequiresReplace()</example>
    </modifier>
    <modifier name="RequiresReplaceIf">
      <use_case>Conditional replacement based on old/new values, when only some changes are destructive.</use_case>
    </modifier>
    <decision_tree name="plan_modifier_selection">
      <question>How does the attribute behave across updates?</question>
      <branch condition="Computed and stable once set">UseStateForUnknown to avoid spurious plan diffs</branch>
      <branch condition="Immutable; backing API cannot update it in place">RequiresReplace</branch>
      <branch condition="Destructive only under some conditions">RequiresReplaceIf with explicit logic</branch>
      <branch condition="Normal mutable attribute">No modifier needed</branch>
    </decision_tree>
  </plan_modifiers>

  <validators>
    <description>Validators reject invalid configuration at plan time with clear diagnostics, before any API call. Prefer the terraform-plugin-framework-validators helper packages (stringvalidator, int64validator, etc.) over hand-rolled logic.</description>
    <common_validators>
      <validator name="stringvalidator.LengthBetween(min, max)">Enforce string length bounds</validator>
      <validator name="stringvalidator.RegexMatches(re, msg)">Enforce a pattern with a human-readable message</validator>
      <validator name="stringvalidator.OneOf(vals...)">Restrict to an enumerated set</validator>
      <validator name="int64validator.Between(min, max)">Enforce numeric range</validator>
    </common_validators>
    <principle>Validate config-time constraints in validators (fast feedback, no side effects). Reserve API round-trips for CRUD.</principle>
  </validators>

  <crud_operations>
    <description>Each CRUD method reads from its typed request and writes to its typed response. Read state/plan with req...Get, write with resp.State.Set, surface problems with resp.Diagnostics.</description>
    <operation name="Create">
      <steps>
        <step>Read the plan into a model struct: req.Plan.Get(ctx, &amp;plan).</step>
        <step>Call the API to create the resource.</step>
        <step>If the create response is empty (common), follow with a Read/GET to populate Computed attributes (uri, ids, keys).</step>
        <step>Write the final state: resp.State.Set(ctx, &amp;plan).</step>
      </steps>
      <note>A "create returns empty body, then GET to hydrate computed fields" flow is a common API shape. Model it explicitly rather than assuming the create response carries all attributes.</note>
    </operation>
    <operation name="Read">
      <description>Refresh state from the real world. This is where drift detection and the critical 404 handling live.</description>
      <critical_pattern name="remove_on_404">
        <description>When the backing object no longer exists (HTTP 404), remove it from state so Terraform plans to recreate it, and return WITHOUT adding an error. Adding an error here would wedge the user.</description>
        <example>
          obj, err := r.client.GetCache(ctx, state.Name.ValueString())
          if err != nil {
              if errors.Is(err, ErrNotFound) { // maps HTTP 404
                  resp.State.RemoveResource(ctx)
                  return
              }
              resp.Diagnostics.AddError("Read failed", err.Error())
              return
          }
          // map obj -> state, then resp.State.Set(ctx, &amp;state)
        </example>
      </critical_pattern>
    </operation>
    <operation name="Update">
      <description>Apply in-place changes. Read plan and prior state, call the update endpoint, then set the new state. Attributes marked RequiresReplace never reach Update (the framework schedules replace instead).</description>
    </operation>
    <operation name="Delete">
      <description>Delete the backing object. On success, the framework automatically removes the resource from state (State.RemoveResource is called on the delete response). Treat a 404 during delete as success (already gone).</description>
    </operation>
  </crud_operations>

  <configure_and_client>
    <description>Configure injects the shared API client built in the provider's Configure into each resource/data source. Always nil-check ProviderData, and type-assert defensively.</description>
    <example>
      func (r *CacheResource) Configure(ctx context.Context, req resource.ConfigureRequest, resp *resource.ConfigureResponse) {
          if req.ProviderData == nil {
              return // framework calls Configure with nil in earlier phases
          }
          client, ok := req.ProviderData.(*APIClient)
          if !ok {
              resp.Diagnostics.AddError(
                  "Unexpected provider data type",
                  fmt.Sprintf("expected *APIClient, got %T", req.ProviderData),
              )
              return
          }
          r.client = client
      }
    </example>
    <note>Identity/account context that many endpoints need (e.g. an account ID resolved from a "current user" lookup) is best resolved once during provider Configure and carried on the client, rather than re-fetched in every CRUD call.</note>
  </configure_and_client>

  <import_state>
    <description>ImportState seeds enough state from an import ID for the subsequent Read to fully hydrate the resource. The simplest form passes the import ID straight into an identifying attribute.</description>
    <example>
      func (r *CacheResource) ImportState(ctx context.Context, req resource.ImportStateRequest, resp *resource.ImportStateResponse) {
          resource.ImportStatePassthroughID(ctx, path.Root("name"), req, resp)
      }
    </example>
  </import_state>

  <http_client>
    <description>API-backed providers need a resilient HTTP layer. Keep retry and error-classification concerns in the client, so CRUD methods stay declarative.</description>
    <retry_policy>
      <rule>Retry on 5xx and 429 (rate limit); do not retry 4xx client errors (except 429).</rule>
      <rule>Exponential backoff (base * 2^attempt) capped at a maximum; honor Retry-After when present.</rule>
      <rule>Respect context cancellation/deadline on every attempt so plan/apply can be interrupted.</rule>
    </retry_policy>
    <error_classification>
      <class code="401/403">Authentication/authorization failure. Surface a clear diagnostic; often a credential-scope problem (see HCL pillar).</class>
      <class code="404">Not found. In Read, remove from state; in Delete, treat as success.</class>
      <class code="429">Rate limited. Retryable with backoff.</class>
      <class code="5xx">Server error. Retryable with backoff.</class>
    </error_classification>
  </http_client>

  <acceptance_testing>
    <description>Acceptance tests exercise real plan/apply/destroy cycles against a live API. They are gated behind the TF_ACC environment variable so they never run during ordinary unit testing.</description>
    <structure>
      <point>Provider factories wire the in-process provider into the test harness. Use ProtoV6ProviderFactories with providerserver.NewProtocol6WithError for protocol 6 (the framework default); ProtoV5ProviderFactories exists for protocol 5 providers and muxed setups.</point>
      <point>Each TestStep supplies HCL Config and a set of Check functions asserting on resulting state.</point>
      <point>PreCheck validates required env (credentials) is present before the test runs.</point>
    </structure>
    <example>
      func TestAccCacheResource_Basic(t *testing.T) {
          resource.Test(t, resource.TestCase{
              PreCheck:                 func() { testAccPreCheck(t) },
              ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
              Steps: []resource.TestStep{
                  {
                      Config: testAccCacheConfig("example-cache"),
                      Check: resource.ComposeTestCheckFunc(
                          resource.TestCheckResourceAttr("example_cache.test", "name", "example-cache"),
                          resource.TestCheckResourceAttrSet("example_cache.test", "uri"),
                      ),
                  },
                  {
                      ResourceName:      "example_cache.test",
                      ImportState:       true,
                      ImportStateVerify: true,
                  },
              },
          })
      }
    </example>
    <run_command>TF_ACC=1 go test -v ./... (acceptance tests create/destroy real resources; expect cost and side effects)</run_command>
  </acceptance_testing>
</provider_development>

<!-- ============================================================= -->
<!-- PILLAR 2: HCL AUTHORING AND OPERATIONS                         -->
<!-- ============================================================= -->

<hcl_and_operations>
  <description>Writing HCL and operating Terraform in practice: lifecycle control, credential-scope diagnosis, composing multi-provider resources, state isolation, and CI validation.</description>

  <lifecycle_ignore_changes>
    <description>ignore_changes tells Terraform to stop reconciling specific attributes after create, so out-of-band changes (made by a CI pipeline, another controller, or the platform itself) do not produce perpetual plan diffs.</description>
    <when_to_use>
      <case>An attribute is set once at creation but subsequently managed elsewhere (e.g. a deploy/build source field updated by an external workflow).</case>
      <case>The platform mutates a field server-side in a way Terraform cannot predict.</case>
    </when_to_use>
    <example>
      resource "hosting_site" "example" {
        name = "example"
        deployment {
          build_type = "workflow"
          domain     = "app.example.com"
        }
        lifecycle {
          ignore_changes = [deployment[0].source]
        }
      }
    </example>
    <caution>ignore_changes is a targeted escape hatch, not a default. Ignoring too much hides real drift. Ignore the narrowest path that stops the false diff.</caution>
  </lifecycle_ignore_changes>

  <credential_scope_troubleshooting>
    <description>A frequent and confusing failure: some resources apply cleanly while others fail at apply with 404 (or 403) on create. When one resource type works and another does not under the same run, suspect the execution credential's scope before suspecting the configuration.</description>
    <diagnostic_procedure>
      <step order="1">Identify which endpoint the failing resource hits (e.g. a user/account-level create) versus the working ones (e.g. a project/repo-scoped create).</step>
      <step order="2">If the failing endpoint is a different privilege tier than the working ones, the token likely lacks that scope. A 404 on create is a common signal that the credential cannot see/act on that endpoint at all.</step>
      <step order="3">Confirm by checking the token's granted scopes against the endpoint's requirement, not by rewriting the resource block.</step>
    </diagnostic_procedure>
    <remedies>
      <remedy>Provision a credential with the required scope for that endpoint, or use a distinct auth strategy for that resource tier.</remedy>
      <remedy>If the capability is not needed, remove the out-of-scope resource from configuration. Removing a resource whose state entry does not exist is safe: it destroys nothing. Verify with state inspection (e.g. terraform state list) that the resource is absent from state before removing its config, so apply completes without a destroy.</remedy>
    </remedies>
    <principle>Separate "the HCL is wrong" from "the credential cannot do this." The second class of failure is invisible in the config and only shows up at apply.</principle>
  </credential_scope_troubleshooting>

  <dns_hosting_composition>
    <description>A recurring shape: exposing a service under a custom domain requires two coordinated resources across two providers — the hosting/platform resource that claims the domain, and a DNS record that routes to the platform's target. Model both; one without the other yields a broken or unverified domain.</description>
    <general_form>
      <part name="hosting_resource">Declares the custom domain on the platform side (a CNAME/custom-domain field on the hosting resource), telling the platform which hostname to serve and to provision a certificate for.</part>
      <part name="dns_record">A CNAME record in the DNS provider pointing the subdomain at the platform's canonical target host.</part>
    </general_form>
    <example>
      # Hosting side: claim the custom domain
      resource "hosting_site" "example" {
        name = "example"
        deployment {
          build_type = "workflow"
          domain     = "sub.example.com"
        }
      }

      # DNS side: route the subdomain to the platform target
      resource "dns_record" "example_sub" {
        zone_id = dns_zone.example.id
        name    = "sub"
        type    = "CNAME"
        content = "example.hosting-platform.net"  # platform's canonical host
        ttl     = 1                                # 1 = automatic
        proxied = false                            # direct CNAME, not proxied
      }
    </example>
    <notes>
      <note>Keep the DNS record unproxied when the platform terminates TLS and validates the domain via a direct CNAME; proxying can break domain verification.</note>
      <note>Provider/service names above (hosting_site, dns_record) are placeholders; the two-resource composition is the reusable idea, applicable to any static-hosting-plus-DNS pairing.</note>
    </notes>
  </dns_hosting_composition>

  <state_and_isolation>
    <description>Organize state so blast radius stays small and plans stay fast.</description>
    <practices>
      <practice priority="high">Isolate independent concerns into separate workspaces/root modules (e.g. DNS, source-control, cloud compute each with their own state), so a plan/apply in one never has to evaluate or risk another.</practice>
      <practice priority="high">Use a remote backend (Terraform Cloud/Enterprise or equivalent) as the single source of truth for state; avoid local state for shared infrastructure.</practice>
      <practice priority="high">Keep secrets out of state and config. Use a secrets manager or an encrypted secrets file (e.g. SOPS) rather than plaintext in .tf files, and mark provider/resource token attributes Sensitive.</practice>
      <practice priority="medium">Group related resources into named files by concern within a project to keep large configurations navigable.</practice>
    </practices>
  </state_and_isolation>

  <ci_validation_chain>
    <description>A dependable CI chain fails fast on cheap checks before spending time on remote operations.</description>
    <chain>
      <step order="1">terraform fmt -check — formatting gate (also enforceable as a pre-commit hook).</step>
      <step order="2">terraform validate — internal consistency, no remote calls.</step>
      <step order="3">tflint — provider-aware and generic lint rules.</step>
      <step order="4">terraform plan — the reviewable artifact; run per isolated project.</step>
      <step order="5">terraform apply — gated behind review/approval; scoped to the changed project.</step>
    </chain>
    <note>For reproducible tool versions in CI, pin Terraform/tofu, tflint, and provider versions declaratively (a Nix/devenv shell entered via `... --command` is one way) so local and CI runs use identical binaries.</note>
  </ci_validation_chain>
</hcl_and_operations>

<context7_integration>
  <description>Use Context7 to confirm current framework symbol names and Terraform option syntax. The framework's public helper packages (stringplanmodifier, stringvalidator, providerserver) differ from older pre-1.0 design documents, so verify before quoting exact names.</description>
  <libraries>
    <library name="Terraform Plugin Framework" id="/hashicorp/terraform-plugin-framework" trust="High" />
    <library name="Terraform Plugin Testing" id="/hashicorp/terraform-plugin-testing" trust="High" />
    <library name="Terraform" id="/websites/developer_hashicorp_terraform" trust="High" />
  </libraries>
  <usage_patterns>
    <pattern topic="plan modifiers">Confirm stringplanmodifier/boolplanmodifier helper names and signatures</pattern>
    <pattern topic="validators">Confirm terraform-plugin-framework-validators package APIs</pattern>
    <pattern topic="acceptance testing">Confirm ProtoV5/V6ProviderFactories and providerserver factory helpers</pattern>
    <pattern topic="lifecycle">Confirm ignore_changes / replace_triggered_by / precondition semantics</pattern>
  </usage_patterns>
</context7_integration>

<best_practices>
  <practice priority="critical">In a provider's Read, remove the resource from state on 404 and return without error, so Terraform recreates rather than wedges.</practice>
  <practice priority="critical">Target new provider work at terraform-plugin-framework, not SDKv2; do not mix their idioms in one resource.</practice>
  <practice priority="high">Mark immutable attributes with RequiresReplace and stable computed attributes with UseStateForUnknown to keep plans accurate and quiet.</practice>
  <practice priority="high">Validate config constraints in validators (fast, side-effect-free); reserve API calls for CRUD.</practice>
  <practice priority="high">Nil-check ProviderData in Configure and type-assert defensively.</practice>
  <practice priority="high">Diagnose apply-time 404/403 by credential scope, not only by configuration.</practice>
  <practice priority="high">Isolate independent infrastructure into separate state/workspaces; keep secrets out of state and config.</practice>
  <practice priority="medium">Use ignore_changes narrowly for attributes managed out-of-band; never as a blanket.</practice>
  <practice priority="medium">Gate acceptance tests behind TF_ACC; run cheap checks (fmt/validate/lint) before plan in CI.</practice>
</best_practices>

<anti_patterns>
  <avoid name="error_on_read_404">
    <description>Returning a diagnostic error from Read when the object is gone (404).</description>
    <instead>Call resp.State.RemoveResource(ctx) and return with no error, so Terraform plans a recreate.</instead>
  </avoid>
  <avoid name="sdkv2_framework_mix">
    <description>Mixing SDKv2 patterns (helper/schema, *schema.ResourceData, CreateContext) into a plugin-framework resource.</description>
    <instead>Use framework interfaces and typed request/response structs consistently.</instead>
  </avoid>
  <avoid name="missing_computed_stability">
    <description>Leaving stable computed attributes without UseStateForUnknown, producing "known after apply" on every plan.</description>
    <instead>Add UseStateForUnknown to computed attributes that do not change once set.</instead>
  </avoid>
  <avoid name="blanket_ignore_changes">
    <description>Wrapping whole blocks in ignore_changes to silence diffs.</description>
    <instead>Ignore the narrowest attribute path that is genuinely managed elsewhere; let real drift surface.</instead>
  </avoid>
  <avoid name="config_blame_for_scope_errors">
    <description>Rewriting resource blocks to chase a 404/403 that is actually a credential-scope limit.</description>
    <instead>Check the token's scope against the failing endpoint; fix credentials or remove the out-of-scope resource (after verifying it is absent from state).</instead>
  </avoid>
  <avoid name="secrets_in_config_or_state">
    <description>Placing tokens/keys in plaintext .tf files or letting them land in state unredacted.</description>
    <instead>Use a secrets manager or encrypted secrets (e.g. SOPS); mark token attributes Sensitive.</instead>
  </avoid>
  <avoid name="single_giant_state">
    <description>One monolithic state for unrelated infrastructure, so every plan evaluates everything.</description>
    <instead>Split into per-concern workspaces/root modules with independent state.</instead>
  </avoid>
  <avoid name="dns_without_hosting_claim">
    <description>Adding a DNS record for a custom domain without claiming the domain on the hosting platform (or vice versa).</description>
    <instead>Provision both the platform custom-domain resource and the DNS record together.</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>In provider Read, RemoveResource on 404 and return without error</rule>
  <rule>Target terraform-plugin-framework for new providers; never mix SDKv2 idioms into a framework resource</rule>
  <rule>Keep secrets out of plaintext config and state; mark token attributes Sensitive</rule>
  <rule>Verify framework symbol names with Context7 before quoting exact helper APIs</rule>
</rules>

<rules priority="standard">
  <rule>Mark immutable attributes RequiresReplace; stable computed attributes UseStateForUnknown</rule>
  <rule>Validate config constraints in validators, not in CRUD</rule>
  <rule>Diagnose apply-time 404/403 as credential scope before rewriting HCL</rule>
  <rule>Use ignore_changes narrowly; isolate state per concern; run fmt/validate/lint before plan</rule>
</rules>

<workflow>
  <phase name="analyze">
    <objective>Classify the task and gather context</objective>
    <step order="1">
      <action>Determine the pillar: provider development (Go) or HCL/operations</action>
      <tool>Parse request; Read relevant .tf or Go files</tool>
      <output>Task classification</output>
    </step>
    <step order="2">
      <action>Identify affected resources, providers, endpoints, or credentials</action>
      <tool>Read, Grep</tool>
      <output>Scope definition</output>
    </step>
    <step order="3">
      <action>Verify current framework/Terraform API names via Context7 when authoring provider code</action>
      <tool>context7 query-docs</tool>
      <output>Confirmed symbol names</output>
    </step>
  </phase>
  <phase name="implement">
    <objective>Write provider code or HCL following the patterns above</objective>
    <step order="1">
      <action>For providers: implement interfaces, schema, plan modifiers, validators, CRUD with 404 handling</action>
      <tool>Edit</tool>
      <output>Provider changes</output>
    </step>
    <step order="1">
      <action>For HCL: compose resources, set lifecycle/ignore_changes narrowly, isolate state, keep secrets external</action>
      <tool>Edit</tool>
      <output>HCL changes</output>
    </step>
  </phase>
  <phase name="validate">
    <objective>Verify correctness</objective>
    <step order="1">
      <action>Providers: go build, go test (unit); TF_ACC=1 go test for acceptance when credentials available</action>
      <tool>Bash</tool>
      <output>Test results</output>
    </step>
    <step order="1">
      <action>HCL: terraform fmt -check, terraform validate, tflint, terraform plan</action>
      <tool>Bash</tool>
      <output>Plan/validation results</output>
    </step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">tflint style warning or fmt drift</example>
    <example severity="medium">Plan shows unexpected diff (often a missing plan modifier or over-broad ignore_changes)</example>
    <example severity="high">Apply fails with 404/403 on some resources — credential scope limit</example>
    <example severity="critical">Plan proposes destroying resources due to state divergence or a RequiresReplace on a data-bearing attribute</example>
  </examples>
</error_escalation>

<constraints>
  <must>Handle 404 in Read by removing from state without error</must>
  <must>Verify framework symbol names against current docs before asserting exact APIs</must>
  <must>Keep secrets out of config and state</must>
  <must>Distinguish credential-scope failures from configuration errors</must>
  <avoid>Mixing SDKv2 and plugin-framework idioms</avoid>
  <avoid>Blanket ignore_changes</avoid>
  <avoid>Monolithic state for unrelated infrastructure</avoid>
</constraints>

<related_skills>
  <skill name="golang-ecosystem">Go language, error handling, module layout, and testing for provider code</skill>
  <skill name="devenv-ecosystem">Reproducible dev shells for Terraform tooling (languages.terraform, git-hooks)</skill>
  <skill name="serena-usage">Navigate provider Go symbols and HCL references efficiently</skill>
  <skill name="context7-usage">Fetch current terraform-plugin-framework and Terraform documentation</skill>
  <skill name="investigation-patterns">Evidence-based diagnosis of plan diffs and apply-time failures</skill>
</related_skills>

<related_agents>
  <agent name="explore">Locate resources, providers, and references across a Terraform project</agent>
  <agent name="quality-assurance">Review provider or HCL changes against this skill guidance</agent>
  <agent name="security">Assess credential scope, secret handling, and state exposure</agent>
  <agent name="devops">CI/CD plan/apply chains and remote backend configuration</agent>
</related_agents>
