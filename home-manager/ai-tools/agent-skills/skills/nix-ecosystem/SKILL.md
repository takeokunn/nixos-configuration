---
name: nix-ecosystem
description: Use when writing Nix expressions, flake.nix, home-manager config, programs.*/services.* modules, nixpkgs packaging, or nix flake check (including vacuous flake checks missing a target platform, overlays vs system packages, and activation-script hazards).
version: 3.0.0
---

Nix traps that read as success. Everything here is a failure that leaves no error behind: a check that verified
nothing, a pin that landed on the wrong version, an option that silently does not exist. Base language
mechanics are assumed; this file carries only what gets it wrong.

## Flakes

### A flake build reads the git tree, not the working tree

`nix build .#pkg`, `nix flake check`, and `nix eval .#x` all resolve `self` through the git-tree fetcher, which
walks `git ls-files` rather than the raw filesystem. A **tracked** file's uncommitted edits are still visible
(Nix prints `warning: Git tree '...' is dirty` to say so) but an **untracked** file does not exist in that tree
at all, no matter what is on disk. A path-based evaluation that never goes through `getFlake`/`fetchTree`
(`nix-instantiate --eval`, `import ./x`, `nix build -f default.nix`) reads the raw filesystem instead, so
untracked files and uncommitted edits are both visible there. Getting this backwards costs real time either
direction: assuming a flake build sees everything on disk, or assuming it sees only what is committed.

The tell for the untracked half: a path referenced from the flake evaluates to `error: opening file '...':
No such file or directory` against a `/nix/store/...-source/` path, even though `ls` on the working tree shows
the file present. Settle which case a command is in with `git status --porcelain -- <path>`: `??` means
untracked and invisible to a flake build; anything else, including a bare `M`, is tracked and visible with its
on-disk content. Observed in this repo: a newly created module directory not yet `git add`ed vanished from
flake evaluation with a "path does not exist" error: the untracked half of this same rule; see the `readDir`
case below.

### `nix flake check` passes vacuously off-platform

Per-system outputs exist only for the systems a flake enumerates, and `nix flake check` evaluates the outputs
present for the *evaluating* system. On a platform the flake never listed there may be nothing at all to check,
and the command exits 0 having verified nothing. An empty check set and a passing one are identical at the exit
code.

The tell: the check succeeds in seconds on a developer machine, then fails in CI on the platform the flake
actually targets.

Treat success on a platform absent from the flake's system list as non-evidence. Run natively on a supported
system, or force full evaluation:

```
nix flake check --all-systems --no-write-lock-file --print-build-logs --keep-going
```

`--all-systems` evaluates every system's outputs; *building* foreign-platform derivations still needs a builder
or substituter. Evaluation-only coverage already catches the common case: an output that does not evaluate on
the target platform. Where a flake exposes Linux outputs only, the authoritative local check on Darwin is the
language toolchain directly, with the flake gate deferred to Linux CI. Say which of the two ran.

### Consume an overlay, not `packages.${system}`

When depending on a package from another flake, prefer its `overlays.default`. The `packages` output is keyed
by system and exists only for the systems upstream enumerated, so `inputs.tool.packages.${system}.default`
fails outright on any other platform. An overlay is system-agnostic by construction: a `final: prev:` function
applied to the *consumer's* package set.

```nix
# Fragile: breaks as soon as the consumer's system is not in upstream's list
packages = [ inputs.tool.packages.${system}.default ];

# Portable: the overlay builds against the consumer's own pkgs
pkgs = import nixpkgs {
  inherit system;
  overlays = [ inputs.tool.overlays.default ];
};
# then simply: pkgs.tool
```

The same asymmetry applies to any system-keyed output (`lib.${system}`, `apps.${system}`). A missing system key
surfaces as `attribute '...' missing` during evaluation, which reads like a typo rather than a platform gap:
check the upstream flake's system list before assuming the attribute path is wrong.

### Pin by enumerating tags, not by "latest release"

On forges that distinguish a git tag from a published Release object, a "latest release" endpoint reports only
tagged commits that also have a Release attached. A maintainer who tags without publishing one is invisible to
it, and the pin silently lands on an older version while looking authoritative.

Resolve versions from tag listings (`git ls-remote --tags <url>`, or the forge's tags endpoint), never from a
latest-release endpoint, and re-verify existing pins the same way when auditing for drift. Only the tag listing
matches what a `github:owner/repo/vX.Y.Z` URL can actually resolve.

### One dependency graph

Every input can declare its own inputs, each resolving independently by default. Without `follows`, `flake.lock`
gains multiple nixpkgs nodes (`nixpkgs`, `nixpkgs_2`, …) pinned to different revisions, producing divergent core
libraries (glibc, openssl, libgit2), binary-cache misses that force rebuilds from source, and occasional ABI
mismatches between components meant to interoperate.

```nix
home-manager = {
  url = "github:nix-community/home-manager";
  inputs.nixpkgs.follows = "nixpkgs";
};

# Deeper override: force a grandchild input to the root as well
some-tool.inputs.helper.inputs.nixpkgs.follows = "nixpkgs";
```

Exceptions are sometimes correct and should not be reflexively "fixed": a component incompatible with unstable
can point at a stable channel, and a tool whose prebuilt closure lives in its own binary cache may need to keep
its own nixpkgs during bootstrap so the cache actually hits. Keep each exception a single isolated node and
record the reason inline with a link to the upstream issue. **The undocumented divergence is the failure mode,
not the divergence.** If you see a package built twice with different hashes, or an unexpected
rebuild-from-source after a flake update, inspect the lock for duplicated nixpkgs nodes.

## Modules

### The option namespace is decided by the importer

`programs.*` and `services.*` exist in both the NixOS and Home Manager module systems, under the same names but
with genuinely different schemas. Which one an option path resolves to is decided by the module system that
evaluated the file, not by the file, its directory, or its author's intent. A tree of "home" modules imported
into a `nixosSystem` evaluates its `programs.foo` against NixOS's module, and any Home-Manager-only option
inside it is an error.

Diagnostic: an option you know the program supports is reported as not existing. Before doubting the option
name, follow the import chain upward to either `nixosSystem` / `nixosModules` (NixOS) or
`homeManagerConfiguration` / `home-manager.users.<name>` (Home Manager). The option is usually fine; the
namespace is the wrong one.

```nix
# Home Manager: prefix/shell exist, plugins take { plugin; extraConfig; } submodules
programs.tmux = {
  prefix = "C-t";
  plugins = [{ plugin = pkgs.tmuxPlugins.resurrect; extraConfig = "set -g @resurrect-strategy-nvim 'session'"; }];
};

# NixOS: no prefix/shell, use shortcut; plugins is a plain package list,
# so per-plugin @plugin-* variables must go into extraConfig instead
programs.tmux = {
  shortcut = "t";
  plugins = with pkgs.tmuxPlugins; [ resurrect ];
  extraConfig = "set -g @resurrect-strategy-nvim 'session'";
};
```

A module tree can land on the NixOS side unintentionally, for example because it also sets `systemd.services`
and was therefore imported as a NixOS module rather than through `home-manager.users.<name>`. Sharing one tree
across both requires either restricting it to options existing in both, or splitting the structurally divergent
programs into per-system files. Assume divergence and check.

### `home.file` makes declared settings read-only

A file placed with `source` or `text` is a symlink into the Nix store, and store files are read-only. For a GUI
application whose settings file is managed this way, every key declared in Nix is enforced on every switch, but
the application's own settings UI can no longer persist changes to those keys, because the write-back fails
against a read-only symlink. **More Nix-declared keys means less in-app control over exactly those toggles.**
Surface this tradeoff *before* declaring more keys, not after the user finds a switch that will not stick.

Some applications rewrite their whole settings file on quit; against a read-only symlink that write fails
outright and can drop unrelated in-app state with it. Prefer a `programs.*` module where one exists, and declare
only the keys that genuinely must be pinned.

A Home Manager module whose settings option has a `freeformType` accepts arbitrary undocumented keys without
validating them, so a typo produces a silently ineffective key rather than an evaluation error. Verify key names
against the application's own documentation.

### Import-from-derivation breaks cross-platform modules

Any `builtins.readFile` / `fromJSON` / `import` applied to a path *inside a derivation output* (for example
`builtins.readFile "${nurPkgs.someTheme}/theme.conf"`) forces that derivation to be built during *evaluation*.
In a module shared across systems this is a trap: evaluating a Darwin configuration that imports the module
triggers an `aarch64-linux` build merely to read a file, so evaluation now needs a Linux builder or fails
outright, and a config that only wanted to render a text file drags in a foreign-platform closure.

**A module intended for cross-platform consumption must be IFD-free.** If one genuinely needs IFD, keep it
vendored per-platform rather than promoting it into shared infrastructure. Make the failure explicit with
`--option allow-import-from-derivation false`; IFD then errors at the offending expression instead of quietly
starting a build. The other tell is an evaluation that pauses to build a derivation whose name carries a system
you are not on.

Reading a file already in the source tree is ordinary evaluation, not IFD, pure and free.

### Two imports that both set `_module.args`

If a bundle module internally imports a companion that sets `_module.args.foo`, importing that same companion
*again* alongside the bundle in the same scope throws `error: attribute '...foo' is defined multiple times`. The
module system does not dedupe two imports that both define a `_module.args` entry. Rely on the ambient one the
bundle already provides. This surfaces at build time, not always at eval time.

### Directory autodiscovery

When a module deploys many files sharing one directory structure, enumerate with `builtins.readDir` instead of
listing each by hand, so adding a file needs no module edit.

```nix
let
  skillFileAttrs =
    let
      entries = builtins.readDir ./skills;
      names = builtins.filter (n: entries.${n} == "directory") (builtins.attrNames entries);
    in
    builtins.listToAttrs (map (name: {
      name = "app/skills/${name}/SKILL.md";
      value = { source = ./skills/${name}/SKILL.md; force = true; };
    }) names);
in
{ xdg.configFile = skillFileAttrs; }
```

`readDir` returns name-to-type (`"directory"` | `"regular"` | `"symlink"`); filter by type rather than assuming
all entries are files. This is exactly the untracked half of the git-tree rule above: "works with `nix build`
locally but the file is missing after switch" is almost always an un-added path.

Modules pulled in via `imports = [ ./x ]` must take `...` in their signature so the module system can pass extra
arguments.

Distribute cross-cutting constants through `specialArgs` (nixosSystem) or `extraSpecialArgs` (standalone
homeManagerConfiguration) rather than relative imports; for a value only some modules need, `_module.args.foo`
sets it locally without threading it through every signature.

Git hooks compose in two layers because git config scoping is local > global: a global layer (Home Manager
`programs.git.hooks` setting `core.hooksPath`) applies everywhere, and a per-project layer overrides it inside
that project. In hook scripts use NUL-delimited iteration (`-z` / `-0` / `--null`) and prefer `printf` to `echo`.

## Packaging

### Derive the version from the manifest

When packaging a repository that already declares its version in a language manifest, derive the Nix `version`
from that manifest. A hardcoded copy drifts silently: the build keeps succeeding while package metadata and the
store path carry a stale version, and nothing ever fails to point it out. Observed in practice: a `flake.nix`
still saying `0.1.0` long after the manifest had reached `0.1.7`.

```nix
let cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in rustPlatform.buildRustPackage {
  pname = cargoToml.package.name;
  version = cargoToml.package.version;
  src = ./.;
  cargoHash = "sha256-...";
}
```

Per-ecosystem equivalents: `fromJSON` of `package.json`, `fromTOML` of `pyproject.toml` (`project.version`), a
parsed `*.cabal` field.

**Not for a foreign release fetched by tag.** There the version is an input rather than an output: keep it a
literal attribute (with `finalAttrs` or `rec` so `rev = "refs/tags/v${version}"` can interpolate it), because
the manifest only becomes readable after the fetch has already been pinned by that same version.

### Rust

`overrideAttrs` cannot change vendored dependencies by setting a new `cargoHash`: it has no effect after the
fact. To override for a version bump you must override the resulting `cargoDeps` and set its `outputHash`.

On nixpkgs 25.05+ the fetchCargoVendor mechanism is the default and non-optional; a bare
`useFetchCargoVendor = true;` is redundant and nixpkgs asks you to remove it. `cargoHash` is still required (or
`cargoLock` for a path-based lockfile). Seed unknown hashes with `lib.fakeHash`, build, then copy the real hash
from the mismatch error. `cmake` is commonly needed transitively (e.g. `aws-lc-sys` via rustls).

### Darwin SDK

Nixpkgs replaced the per-framework Darwin inputs with a bundled versioned SDK. Drop unversioned
`darwin.apple_sdk.frameworks.*` entries entirely (the default SDK is in the Darwin stdenv now) and add
`apple-sdk_NN` to `buildInputs` only when a specific version is required. The SDK propagates libiconv/libresolv
automatically. The legacy `darwin.apple_sdk_11_0.*` stubs have been removed and now error out.

### JavaScript builder selection

| Situation | Builder |
|---|---|
| npm lockfile, no build step | `buildNpmPackage` with `npmDepsHash`, `dontNpmBuild = true` |
| npm lockfile, needs build | `buildNpmPackage` with `npmDepsHash`; `finalAttrs` when src needs the version |
| pnpm lockfile | `stdenvNoCC.mkDerivation` + `fetchPnpmDeps` (hash + fetcherVersion) + `pnpmConfigHook`: the pnpm major used to fetch deps must match the one used to build |
| bun lockfile | Two-phase: a fixed-output derivation runs `bun install --frozen-lockfile`, then the main build consumes it |
| turbo / monorepo orchestrator | See the daemon trap below |

Use `nodejs-slim_NN` for the runtime wrapper and full `nodejs_NN` for the build; wrap entrypoints with
`makeBinaryWrapper`; install app trees under `$out/lib/<pname>/`.

### Build daemons break sandbox teardown

Monorepo tools that spawn a background daemon holding file locks on cache directories (turbo and similar) break
sandbox teardown with `cannot unlink "...drv-0": Directory not empty`, because the daemon is still alive and its
cache tree is locked when the sandbox tries to clean up.

Disable by both environment variable and CLI flag, force cache bypass, and remove the cache directories at the
end of buildPhase: `export TURBO_DAEMON=0; export TURBO_FORCE=true; pnpm turbo build --no-daemon;` then
`rm -rf .turbo node_modules/.cache/turbo || true`. **Any build helper that persists a daemon or on-disk cache
across invocations must be forced into single-shot, cache-clean mode inside the sandbox.**

### Hashes

Compute all hashes as SRI (`sha256-...`). Language builders carry a second hash for the dependency set
(`cargoHash`, `vendorHash`, `npmDepsHash`, `pnpmDeps.hash`) that must be regenerated whenever the lockfile
changes. Consult Context7 with library ID `/nixos/nixpkgs` for current language-builder patterns before writing
a language-specific derivation; prefer the language builder to raw `mkDerivation`, and keep `meta` complete
(description, homepage, license, maintainers, platforms, `mainProgram` for CLIs).

## Secrets never enter the store

The store is world-readable by design (0555 directories, 0444 files) and content-addressed. Anything
materialized into it (via `writeText`, `toJSON` piped into a store file, an unquoted path literal, or
string-interpolating a path) becomes readable by every local user *and* is copied into any binary cache the
closure is pushed to. **No plaintext or ciphertext secret should ever enter the store.**

A path literal, or a string produced by coercing a path (`"${./secrets.yaml}"`,
`"${inputs.self}/secrets.yaml"`), is copied into the store as an evaluation input: even a store-resident sops
file is then in the closure. Reference the secret by a runtime string path that is never a Nix path type: an
absolute string like `"/var/lib/app-secrets/secrets.yaml"` resolving only on the target. With sops-nix this also
needs `sops.validateSopsFiles = false;`, because the validator asserts `builtins.isPath sopsFile` and rejects a
plain string.

Keep decryption keys out of source and store by deriving them at boot: a oneshot systemd service (`Type =
oneshot`, `RemainAfterExit = true`, ordered before the units that need it and after host-key generation) derives
an age key from the host SSH key with `ssh-to-age`, writes it 0600, and decrypted secrets land under a runtime
tmpfs. Make the derivation idempotent.

Prove nothing leaked:

```
nix-store -q --requisites <drvPath-or-outPath> | grep -i secrets
```

An empty result is the pass condition. Run it whenever a config touches secret handling, and always before
pushing a closure to a public cache.

Declarative modules that serialize their whole config into the store (`writeText` of a `toJSON` blob) turn any
`env`, `vars`, command strings, or hook bodies into world-readable store data. Treat such modules as
non-secret-only unless reworked.

## Activation scripts are a privileged, recurring attack surface

Activation scripts (nix-darwin and NixOS alike) run as root, and many write into the logged-in user's home:
SSH material, agent sockets, per-user state. The user owns every component of that path, so anything running as
that user can replace any directory along it with a symlink in the window between activation checking the path
and writing to it. Path-based operations re-resolve the whole path on every call, so each call reopens the race;
the check you performed a moment ago says nothing about the object you are about to write. **This surface exists
on every switch, by construction**, which is what distinguishes it from ordinary untrusted-input handling.

- Never use path-based `mkdir -p`, `chmod`, `chown`, `touch`, or `install` on user-controlled paths from a
  privileged activation script or a root helper it installs.
- `O_NOFOLLOW` on the final component alone is insufficient: an attacker only needs an ancestor. Traverse
  descriptor-relatively: open each component with `openat(dirfd, name, O_DIRECTORY | O_NOFOLLOW)` and operate on
  the resulting descriptor (`mkdirat`, `openat`, `fchown`, `fchmod`), so the object you validated is the object
  you write.
- Validating a descriptor and then invoking a subprocess with the original path throws the guarantee away: the
  subprocess re-resolves from scratch and reintroduces the race in full. Do the work in-process against the
  descriptor, or pass the descriptor itself (`/dev/fd/N`) where the tool accepts it.

On macOS several standard paths are system symlink aliases into `/private` (`/var`, `/tmp`, `/etc`). A blanket
`O_NOFOLLOW` traversal therefore rejects legitimate configuration such as a state directory under `/var/lib/…`,
and the naive fix (drop `O_NOFOLLOW`) undoes the hardening. Canonicalize only an explicit allowlist of these
fixed system aliases and keep strict traversal for everything else.

The same reasoning covers any privileged helper a Nix module installs. For general untrusted-input rules see
[trust-boundaries](../trust-boundaries/SKILL.md); what is Nix-specific here is that activation is privileged by
construction and re-runs on every switch.

## Darwin building Linux

Apple Silicon macOS cannot natively build `*-linux` derivations. Register a Linux builder VM as a build machine;
for anything large, offload to native CI and only substitute the result.

| Need | Mechanism |
|---|---|
| Simplest, zero extra inputs | nix-darwin's `nix.linux-builder` (QEMU). Auto-configures buildMachines, distributedBuilds, builders-use-substitutes. Slowest, least to maintain. |
| Faster boot, native virtio | A MicroVM framework or a vfkit runner over Apple's hypervisor. vfkit is the practical macOS hypervisor (built-in virtiofs, no 9p, no TAP); microvm.nix on Darwin needs a compatible pin because virtiofsd is unavailable on some revisions, plus `storeOnDisk = false` and `vmHostPackages = nixpkgs.legacyPackages.aarch64-darwin`. |
| Near-native | A Virtualization.framework builder VM. No QEMU overhead, and Rosetta gives x86_64-linux at roughly 70–90% native versus QEMU's order-of-magnitude slowdown. Most bespoke. |

Only one mechanism can be active at once: they are mutually exclusive on `nix.buildMachines` /
`nix.linux-builder`.

On the native-framework path: networking is NAT/user-mode only, with no TAP and no port forwarding, so discover
the guest IP via the ARP table (deterministic guest MAC) or `/var/db/dhcpd_leases` (fixed guest hostname).
`/nix/store` is shared read-only over virtiofs with an overlay for the writable layer. There is no nested
virtualization on Apple Silicon, so the builder VM will not run inside another VM: it fails in typical M-series
CI runners.

Generate the builder SSH keypair at activation time and keep the private key on the host; never commit it, since
the config repo may be public. Share only the public key into the guest over a read-only mount. Bootstrap may
use a password, but harden to key-only once the builder works.

Substituting foreign-platform derivations from Darwin needs explicit flags, because NixOS `system.build.toplevel`
sets `preferLocalBuild` (`allowSubstitutes = false`) and the daemon negatively caches narinfo 404s:

```
nix build .#packages.aarch64-linux.<pkg> \
  --max-jobs 0 \
  --option extra-platforms aarch64-linux \
  --option always-allow-substitutes true
```

Add `--option narinfo-cache-negative-ttl 0` when a stale negative cache hides a now-available path. Permanent
form: `nix.settings = { extra-platforms = [ "aarch64-linux" ]; always-allow-substitutes = true; };`. A closure
often spans multiple substituters, so all must be configured for the fetch to complete.

For large closures, build on native CI and let the Mac substitute the finished closure. Building a big `*-linux`
closure on a virtiofs-overlay store can churn the store hard enough to lose store-path visibility mid-build, an
architectural limit of the shared/overlay store, not a transient bug.

### launchd

Run a long-lived agent in the correct domain. A daemon needing a graphical session must live in the `gui`
domain; leaving it in the background/user domain can inject `LimitLoadToSessionType = Background` and produce
flaky bootstrap where a process is alive but `launchctl print gui/$UID/...` shows nothing loaded. When migrating
from a user-domain job, explicitly boot out the old `user/$UID/...` job or launchd may respawn it.

Raise file-descriptor limits in layers or you hit "Too many open files": a boot-time daemon for `kern.maxfiles`
/ `kern.maxfilesperproc` / `launchctl limit maxfiles`, per-agent `NumberOfFiles` (soft and hard), and (the
commonly missed one) the GUI session's own `launchctl limit maxfiles`.

### PID files and GC roots

A PID file tells you *a* process exists, not that it is *your* process. PIDs are reused, and the state file can
be replaced between the read and the signal. Persist a composite identity (canonical executable path, canonical
state directory, process start time) and require all three to match before acting. Re-read and revalidate
immediately before every signal, including the SIGKILL fallback of a stop handshake: a slow shutdown escalation
is exactly the window in which the original process exits and an unrelated one inherits the PID.

A long-lived VM or daemon whose closure must survive garbage collection needs an indirect GC root, and refreshing
it must never leave a window with no root. Create the replacement symlink under a temporary name and `rename` it
over the old one; never remove the valid root first, or a concurrent `nix-collect-garbage` can delete the closure
of a running system. Accept only canonical `/nix/store/...` values as root targets, bound any external
`nix-store` invocation with a timeout, and on expiry send SIGTERM, escalate to SIGKILL, and always reap the child.

## Deployment

NixOS activation (`switch-to-configuration`, as driven by deploy-rs) takes a non-blocking exclusive flock at
`/run/nixos/switch-to-configuration.lock`. A second concurrent activation fails immediately (exit 11 / EAGAIN)
rather than queueing. A local Ctrl+C on deploy-rs can leave the remote `switch-to-configuration` running and
holding the lock; kill the leftover remote process. A crash while holding it leaves a stale lock file to remove
manually: check with `fuser` / `ps` on the target before assuming it is stale.

deploy-rs `magicRollback` verifies post-activation reachability, but when activation starts many or slow
services, startup can exceed the verification window and trigger a false rollback. Disable `magicRollback`, rely
on `autoRollback`, and raise `activationTimeout`:

```nix
{ autoRollback = true; magicRollback = false; activationTimeout = 600; }
```

Keep ephemeral containers' `TimeoutStopSec` low (e.g. 15s) so a slow shutdown does not hold the activation lock;
with the default 90s across many containers, teardown alone can block the next deploy for a long time. Declared
NixOS containers are auto-started by systemd on activation, and a full reset of systemd-machined state before
applying can be needed to avoid EEXIST races on redeploy.

## Sandbox discipline

The build sandbox has no network, no conventional absolute paths (no `/bin/sleep`, no `/bin/echo`), and a
minimal toolset (no git); Nix also builds Rust in the release profile.

- Tests needing network or git must be `#[ignore]`d or feature-gated so the sandboxed `cargo test` still passes;
  run them outside the sandbox in a dev shell.
- The release profile strips `debug_assert!`, so a `#[should_panic]` test asserting that a `debug_assert!` fires
  must be gated with `#[cfg(debug_assertions)]`, or it fails under the Nix build while passing under a debug
  `cargo test`.
- A hardcoded conventional path (`/bin/sleep`, `/bin/echo`, `/usr/bin/env`) does not exist in the sandbox;
  only `PATH` entries from `buildInputs` and their store paths do. Such a test passes on the host and fails only
  under `nix build` / `nix flake check`, and the failure can present as a bare nonzero exit with the underlying
  signal swallowed, so the symptom itself never names the missing binary. Resolve every tool through `PATH`
  (bare `sleep`, `echo`) or an explicit store path from `buildInputs` (`"${coreutils}/bin/sleep"`), never a
  conventional absolute path. The same rule holds for any other hermetic runner (Bazel, a minimal container
  image) wherever the toolset is not the host's.

### The multicall `argv[0]` trap

In a Nix environment a large share of `PATH` entries are symlinks into multicall binaries: coreutils installs
`echo`, `ls`, `cat` and dozens more as links to a single `coreutils` executable that dispatches on `argv[0]`.
Code that "hardens" a resolved executable path by canonicalizing it (`realpath`, `readlink -f`,
`Path::canonicalize`) rewrites `argv[0]` to the dispatcher's own name, and the program then runs as the wrong
tool entirely. The signature is a spawn producing empty stdout, a usage message on stderr, and a nonzero exit:
a failure that looks like a broken argument list, not a resolution bug.

**Existence and executability checks may follow symlinks; the path handed to exec must keep the candidate's own
basename. Canonicalize for comparison, never for invocation.**

This is near-certain to surface under `nix build` / `nix flake check` and may never surface on a distribution
shipping separate binaries, a textbook "passes on my machine, fails in the sandbox" case. It is plain POSIX
behavior; Nix just makes the multicall layout the norm.

## Rules

- Verify `flake.lock` is updated after any input change.
- Use `mkIf` and `mkMerge` for conditional module options, never a plain `if` at top level. Prefer `lib.mkDefault`
  to `lib.mkForce`.
- Set `home.stateVersion` to the initial version and do not change it after setup unless migrating.
- Always provide a hash to `fetchurl` / `fetchTarball`; use `nix-prefetch-url` or `lib.fakeHash` to obtain it.
- Avoid `with lib;` at module top level: it pollutes scope and hides where functions come from. Use explicit
  `lib.mkIf`, `lib.types.str`. Narrow scopes like `with pkgs; [ ... ]` are fine.
- Prefer declarative configuration to `nix-env -i`, `nix-channel`, and other mutable `/nix/var` state.
- Document non-obvious overlay and override rationale in a comment: this is one of the cases where a comment
  carries a WHY the code cannot.

## Related

- [trust-boundaries](../trust-boundaries/SKILL.md): general untrusted-input and privilege-boundary rules
- [context7-usage](../context7-usage/SKILL.md): fetching current nixpkgs and Home Manager documentation
- [investigation-patterns](../investigation-patterns/SKILL.md): debugging evaluation and derivation failures
- [serena-usage](../serena-usage/SKILL.md): navigating Nix expressions by symbol
- [testing-patterns](../testing-patterns/SKILL.md): what acceptance means for a declarative change
- Language conventions for nixpkgs packaging: [rust-ecosystem](../rust-ecosystem/SKILL.md) and
  [common-lisp-ecosystem](../common-lisp-ecosystem/SKILL.md)
