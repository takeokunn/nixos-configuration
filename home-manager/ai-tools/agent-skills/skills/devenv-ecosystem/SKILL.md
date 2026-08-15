---
name: devenv-ecosystem
description: Use when working with devenv.nix, languages.*/services.* options, git-hooks, devenv shell/up/build, or devenv development environments.
version: 3.0.0
---

Patterns for devenv configuration — language setup, services, tooling (git-hooks, scripts, processes),
outputs, and profiles. For Nix language syntax, flake structure, or general NixOS module patterns, see
nix-ecosystem instead; this skill covers only what is devenv-specific.

## devenv 2.0

Released March 2026. Two changes affect existing configs directly:

- **The native process manager is now the default**, replacing process-compose. A config that assumes
  process-compose behavior (health checks, `depends_on`, readiness probes) silently gets the native manager
  instead unless `process.manager.implementation = "process-compose";` is set explicitly — there is no error,
  just different runtime behavior for `devenv up`. To keep process-compose:

  ```nix
  process.manager.implementation = "process-compose";

  processes.api = {
    exec = "cargo run";
    process-compose.readiness_probe.http_get = {
      host = "localhost";
      port = 8080;
      path = "/health";
    };
  };
  ```

- devenv 0.x is deprecated; support drops in devenv 3.

Other 2.0 changes are cosmetic to the config author: a live TUI on every command, background rebuilds in the
native shell (save a file, `Ctrl+Alt+R` applies the new environment without leaving the shell), a C FFI
backend replacing spawned `nix` subprocesses, an incremental per-attribute eval cache, and cross-repo output
references.

## devenv vs. flakes

Use devenv over pure flakes when you need `services.*` (simpler than hand-rolled service derivations),
git-hooks integration, process supervision, or `languages.*` version management. Use flakes directly for pure
derivations or NixOS system configuration — devenv adds overhead there.

## Multi-shell and custom outputs (2.0+)

`devenv.shells.<name>` defines multiple shell configurations in one file — each is a full devenv module, not
an overlay of the default:

```nix
{ pkgs, ... }:
{
  devenv.shells.default = {
    languages.python.enable = true;
    packages = [ pkgs.git ];
  };
  devenv.shells.ci = {
    languages.python.enable = true;
    env.CI = "true";
  };
}
```

`devenv.outputs` exposes arbitrary flake outputs (packages, checks) from within the module:

```nix
{ pkgs, config, ... }:
{
  devenv.outputs = {
    packages.my-app = config.languages.rust.import ./. {};
    checks.lint = pkgs.runCommand "lint" {} ''
      ${pkgs.clippy}/bin/cargo-clippy
      touch $out
    '';
  };
}
```

Language modules expose an `import` builder for packaging an app with the same toolchain the shell uses —
`config.languages.rust.import ./rust-app {}` — instead of hand-writing a `stdenv.mkDerivation`.

## Profiles (1.9+)

Three distinct mechanisms, easy to conflate:

- **Value override** — `profiles."python-3.12".config = { languages.python.version = "3.12"; };`, selected
  with `devenv shell --profile python-3.12`.
- **Composable modules** — `profiles.backend.module = { ... }`, `profiles.frontend.module = { ... }`, then
  `profiles.fullstack.extends = [ "backend" "frontend" ];` to combine them.
- **Automatic selection by environment** — `profiles.hostname."ci-server".module` and
  `profiles.user."developer".module` apply based on the machine's hostname or the invoking user, with no
  explicit `--profile` flag needed. This is a silent-selection mechanism: a shell picks up the profile because
  of who or where it's running, not because of anything in the invocation — worth checking for before
  debugging "works on my machine" config drift.

## git-hooks

`git-hooks.hooks` wires pre-commit hooks (formatters: `nixfmt-rfc-style`, `prettier`, `black`, `rustfmt`,
`ormolu`, `gofmt`; linters: `eslint`, `clippy`, `shellcheck`, `yamllint`, `actionlint`, `hadolint`,
`markdownlint`; security/hygiene: `detect-private-keys`, `check-merge-conflicts`, `check-case-conflicts`,
`editorconfig-checker`; `treefmt` as a universal formatter wrapper). Custom hooks take an `entry` shell
command and a `files` regex:

```nix
git-hooks.hooks.my-custom-hook = {
  enable = true;
  entry = "${pkgs.bash}/bin/bash -c 'echo Running custom hook'";
  files = "\\.nix$";
  pass_filenames = true;
};
```

## Scripts, tasks, processes

`scripts.<name>.exec` puts a command on `$PATH` inside the shell; `tasks.<name>` adds dependency ordering via
`before`/`after`, including hooking into shell entry itself (`before = [ "devenv:enterShell" ];`) — a task can
run automatically on every `devenv shell` invocation this way, which is easy to miss when reading a config
that otherwise looks static.

`processes.<name>.exec` are managed by whichever process manager is active (native by default in 2.0, see
above). Keep `enterShell` fast and offline-capable — it runs on every shell entry — and push network-dependent
or slow setup into a `task` with an explicit trigger instead.

## Environment and secrets

`dotenv.enable = true;` (with `dotenv.filename` to pick a non-default file) loads `.env`-style files that
should stay out of version control. Don't put real secrets in the `env` attribute directly — it lands in the
Nix store, which is world-readable on the local machine.

## Context7

Library id `/cachix/devenv` (trust score 9.7, ~1354 snippets) — use for current `languages.*`/`services.*`
option shapes, available git-hooks, and the devenv 2.0 migration guide, since option surfaces here change
between minor versions faster than this file is updated.

## Related

- [nix-ecosystem](../nix-ecosystem/SKILL.md) — Nix language syntax, flake structure and inputs, general NixOS
  module patterns, `lib.*` functions; devenv builds on all of it but doesn't re-teach it.
- [serena-usage](../serena-usage/SKILL.md) — symbol operations for navigating devenv configurations.
- [context7-usage](../context7-usage/SKILL.md) — fetching current devenv documentation via Context7 MCP.
