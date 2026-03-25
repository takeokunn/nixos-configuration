---
name: devenv-ecosystem
description: "Use when working with 'devenv', 'devenv.nix', 'languages.*', 'services.*', 'git-hooks', 'devenv shell', 'devenv up', 'devenv build', or devenv development environments. Provides devenv configuration patterns for language setup, services, git-hooks, processes, outputs, and profiles."
---

Comprehensive patterns for devenv configuration: language modules, service management, git-hooks, scripts, processes, outputs, and profiles. For Nix language syntax and flake structure, defer to the nix-ecosystem skill.

## CLI Commands

| Command | Purpose |
|---------|---------|
| `devenv init` | Initialize new project with devenv.nix and .envrc |
| `devenv shell` | Enter development shell with configured tools |
| `devenv up` | Start all configured processes (process-compose) |
| `devenv build` | Build configured outputs |
| `devenv test` | Run configured tests |
| `devenv tasks run` | Execute configured tasks |
| `devenv update` | Update devenv inputs |
| `devenv gc` | Garbage collect unused derivations |
| `devenv info` | Show environment information |
| `devenv container` | Build OCI container images |

## Core Configuration

```nix
{ pkgs, config, ... }:
{
  languages.python.enable = true;
  packages = [ pkgs.git ];
  services.postgres.enable = true;
}
```

## Language Configuration

### Common Patterns

```nix
# Basic enable
languages.python.enable = true;

# Version specification
languages.python.version = "3.12";
languages.rust.channel = "stable"; # or "nightly", "beta"

# Package override
languages.go.package = pkgs.go_1_22;

# Version file
languages.ruby.versionFile = "./.ruby-version";

# Dev tooling (devenv 1.8+)
languages.rust.dev = {
  lsp.enable = true;
  debugger.enable = true;
  linter.enable = true;
  formatter.enable = true;
};
```

### Language Categories

**Systems**: C, C++, Rust, Go, Zig, Nim, Odin, V

```nix
languages.rust = {
  enable = true;
  channel = "stable";
  components = [ "rustc" "cargo" "clippy" "rustfmt" "rust-analyzer" ];
  targets = [ "wasm32-unknown-unknown" ];
};
```

**Interpreted**: Python, Ruby, PHP, Perl, Lua

```nix
languages.python = {
  enable = true;
  version = "3.12";
  uv.enable = true;
  uv.sync.enable = true;
};
```

**JVM**: Java, Kotlin, Scala, Clojure

```nix
languages.java = {
  enable = true;
  jdk.package = pkgs.jdk21;
  maven.enable = true;
};
```

**Functional**: Haskell, OCaml, Elixir, Erlang, Elm, PureScript, Gleam, Idris, Lean4

**Web**: JavaScript/TypeScript/Deno with npm/pnpm/bun/yarn/corepack

```nix
languages.javascript = {
  enable = true;
  package = pkgs.nodejs_22;
  pnpm.enable = true;
  pnpm.install.enable = true;
};
```

### Package Manager Selection (JS/TS)

- Existing project: Match lock file (package-lock.json=npm, pnpm-lock.yaml=pnpm, yarn.lock=yarn, bun.lockb=bun)
- New project needing speed: pnpm or bun
- Maximum compatibility: npm
- Monorepo: pnpm or yarn with workspaces

## Services

### Databases

```nix
services.postgres = {
  enable = true;
  package = pkgs.postgresql_16;
  initialDatabases = [{ name = "myapp"; }];
  listen_addresses = "127.0.0.1";
  port = 5432;
  extensions = [ "postgis" "pgvector" ];
};

services.redis = { enable = true; port = 6379; };
```

Available: postgres, mysql, mongodb, redis, memcached, minio, elasticsearch, opensearch, rabbitmq, kafka, nginx, caddy, varnish, mailhog, mailpit.

## Git Hooks

```nix
git-hooks.hooks = {
  nixfmt-rfc-style.enable = true;
  prettier.enable = true;
  eslint.enable = true;
  clippy.enable = true;
  shellcheck.enable = true;
  detect-private-keys.enable = true;
  check-merge-conflicts.enable = true;
};

# Custom hook
git-hooks.hooks.my-custom-hook = {
  enable = true;
  name = "my-custom-hook";
  entry = "${pkgs.bash}/bin/bash -c 'echo Running custom hook'";
  files = "\\.nix$";
};
```

## Scripts and Processes

```nix
scripts = {
  build.exec = "cargo build --release";
  deploy = {
    exec = "kubectl apply -f k8s/";
    description = "Deploy to Kubernetes";
  };
};

processes = {
  web.exec = "npm run dev";
  api.exec = "cargo run";
};
```

Advanced process-compose with health checks:

```nix
processes.web = {
  exec = "npm run dev";
  process-compose = {
    depends_on.api.condition = "process_healthy";
    readiness_probe.http_get = { host = "localhost"; port = 3000; };
  };
};
```

## Tasks (devenv 1.7+)

```nix
tasks = {
  "build:frontend" = { exec = "npm run build"; before = [ "devenv:enterShell" ]; };
  "build:backend" = { exec = "cargo build --release"; before = [ "devenv:enterShell" ]; };
  "build:all" = { exec = "echo Build complete"; after = [ "build:frontend" "build:backend" ]; };
};
```

## Outputs (devenv 1.6+)

```nix
outputs = {
  rust-app = config.languages.rust.import ./rust-app {};
  python-app = config.languages.python.import ./python-app {};
};
```

## Profiles (devenv 1.9+)

```nix
profiles = {
  backend.module = {
    languages.rust.enable = true;
    services.postgres.enable = true;
  };
  frontend.module = {
    languages.javascript.enable = true;
    languages.typescript.enable = true;
  };
  fullstack.extends = [ "backend" "frontend" ];
};
# Usage: devenv shell --profile backend
```

## Workflow

1. **Analyze**: Identify required languages/versions, needed services, processes/scripts, team profiles
2. **Implement**: Start minimal (packages, primary language), add services, configure git-hooks, add scripts/processes/profiles
3. **Validate**: Run `devenv shell`, `devenv up`, test git hooks, verify scripts

## Anti-Patterns to Avoid

- **Hardcoded versions** -- use `languages.*.version` or `languages.*.package`
- **Shell commands for installation** -- use `packages = [ pkgs.name ];`
- **Slow enterShell** -- move slow operations to tasks or scripts
- **Mixing package managers** -- choose one (npm OR pnpm OR yarn OR bun)
- **Secrets in env** -- use `dotenv.enable` with `.env` files in `.gitignore`

## Best Practices

- Use `languages.*.enable` instead of adding packages directly
- Keep `enterShell` fast and offline-capable
- Use `services.*` instead of running databases manually
- Use `git-hooks` for consistent code quality
- Document scripts with `description` attribute
- Use `dotenv.enable` for environment-specific configuration
