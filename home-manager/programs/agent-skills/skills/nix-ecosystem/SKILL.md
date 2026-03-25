---
name: nix-ecosystem
description: "Use when the agent needs to 'write nix', 'nix expression', 'flake.nix', 'home-manager config', 'programs.*', 'services.*', 'nixpkgs packaging', 'buildGoModule', or 'buildRustPackage'. Provides comprehensive Nix language, flakes, Home Manager, and nixpkgs packaging patterns."
---

Comprehensive patterns for Nix language, flakes, Home Manager configuration, and nixpkgs packaging.

## Core Concepts

- **Lazy evaluation**: Expressions computed only when needed
- **Pure functions**: Same inputs always produce same outputs; use derivations for build actions
- **Attribute sets**: Primary data structure (`{ attr = value; }`, access with `set.attr`)
- **Reproducibility**: All builds are deterministic; avoid impure paths

## Development Workflow

1. **Analyze**: Identify target (flake, module, derivation), check existing patterns, consult Serena memories
2. **Implement**: Follow decision trees, use appropriate `lib` functions, apply best practices
3. **Validate**: `nix flake check`, `nix eval`, `nix build`

## Nix Language Patterns

### let-in, with, inherit

```nix
# Local bindings
let
  helper = x: x + 1;
  value = helper 5;
in value * 2

# Bring set into scope (avoid nesting)
with pkgs; [ git vim tmux ]

# Copy attributes
{ inherit (pkgs) git vim; inherit name version; }
```

### Overlays and callPackage

```nix
# Overlay: modify/extend nixpkgs
final: prev: { myPackage = prev.myPackage.override { ... }; }

# callPackage: dependency injection
myPackage = pkgs.callPackage ./package.nix { };
```

### mkDerivation

```nix
pkgs.stdenv.mkDerivation {
  pname = "mypackage";
  version = "1.0.0";
  src = fetchFromGitHub { owner = "user"; repo = "pkg"; rev = "v1.0.0"; hash = "sha256-..."; };
  nativeBuildInputs = [ pkgs.cmake ];  # build-time tools
  buildInputs = [ pkgs.openssl ];       # runtime libraries
  installPhase = ''
    mkdir -p $out/bin
    cp mypackage $out/bin/
  '';
}
```

### Module Options

```nix
{ config, lib, pkgs, ... }: {
  options.myModule = {
    enable = lib.mkEnableOption "my module";
    setting = lib.mkOption {
      type = lib.types.str;
      default = "value";
      description = "A setting";
    };
  };
  config = lib.mkIf config.myModule.enable { /* ... */ };
}
```

## Flakes

### Basic Structure

```nix
{
  description = "Project description";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";  # share input
    };
  };
  outputs = { self, nixpkgs, ... }: { /* packages, devShells, etc. */ };
}
```

### Common Outputs

| Output | Purpose | Example |
|--------|---------|---------|
| `packages.<system>.default` | `nix build` | `pkgs.hello` |
| `devShells.<system>.default` | `nix develop` | `pkgs.mkShell { packages = [...]; }` |
| `apps.<system>.default` | `nix run` | `{ type = "app"; program = "..."; }` |
| `overlays.default` | Extend nixpkgs | `final: prev: { ... }` |
| `nixosModules.default` | NixOS modules | `{ options = ...; config = ...; }` |
| `nixosConfigurations.host` | Full system | `nixpkgs.lib.nixosSystem { ... }` |
| `homeConfigurations."user@host"` | HM config | `home-manager.lib.homeManagerConfiguration { ... }` |

### Per-System Pattern

```nix
let
  systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
  forAllSystems = nixpkgs.lib.genAttrs systems;
in {
  packages = forAllSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in { default = pkgs.hello; }
  );
}
```

### Flake Commands

`nix flake update`, `nix flake update input-name`, `nix flake show`, `nix flake check`

## Home Manager

### Module Structure

```nix
{ config, pkgs, lib, ... }: {
  options.custom.feature.enable = lib.mkEnableOption "feature";
  config = lib.mkIf config.custom.feature.enable { /* ... */ };
}
```

### Program Configuration

```nix
# Use programs.* when available (preferred)
programs.git = {
  enable = true;
  userName = "name";
  userEmail = "email";
  extraConfig = { core.editor = "nvim"; init.defaultBranch = "main"; };
};

# Manual dotfile management when no module exists
home.file.".config/app/config".text = ''
  key = value
'';
xdg.configFile."app/config".source = ./config;

# Environment
home.sessionVariables = { EDITOR = "nvim"; };
home.sessionPath = [ "$HOME/.local/bin" ];
```

### Common Modules

`programs.git`, `programs.neovim`, `programs.fish`, `programs.tmux`, `programs.direnv` (with `nix-direnv.enable = true`)

### Best Practices

- Use `programs.*` when available instead of manual configuration
- Set `home.stateVersion` to initial HM version; do not change unless migrating
- Group related configurations in separate modules
- Use `lib.mkIf` for conditional configuration
- Prefer `xdg.configFile` over `home.file` for XDG-compliant apps

## NixOS Integration

```nix
# Home Manager as NixOS module
{
  imports = [ home-manager.nixosModules.home-manager ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.username = import ./home.nix;
}
```

## Nixpkgs Packaging

The agent should use Context7 with `/nixos/nixpkgs` for up-to-date packaging patterns.

| Language | Builder | Context7 Query |
|----------|---------|---------------|
| Go | `buildGoModule` | "buildGoModule" |
| Rust | `rustPlatform.buildRustPackage` | "rustPlatform.buildRustPackage" |
| Haskell | `haskellPackages.mkDerivation` | "haskellPackages" |
| PHP | `php.buildComposerProject2` | "buildComposerProject2" |
| C/C++ | `stdenv.mkDerivation` + cmake/meson | "cmake" or "meson" |

## Anti-Patterns

- **Impure paths**: Use `fetchurl`, `fetchFromGitHub`, or relative paths
- **Nested with**: Prefer explicit `pkgs.git` for readability
- **rec overuse**: Use `let-in` for complex recursive definitions
- **String interpolation abuse**: Use `lib` functions for path manipulation

## Error Escalation

| Severity | Example | Action |
|----------|---------|--------|
| Low | Style inconsistency | Suggest formatting |
| Medium | Evaluation error | Debug with `--show-trace`, fix expression |
| High | Build failure | Analyze build log, present options |
| Critical | Impure expression breaking reproducibility | Block, require pure alternatives |

## Critical Rules

- Use `lib` functions for complex operations
- Follow project's existing Nix patterns
- Maintain reproducibility in all expressions
- Always consult Context7 for latest nixpkgs packaging patterns

## Related Skills

- `serena-usage`: Navigate Nix expressions and module definitions
- `investigation-patterns`: Debug evaluation errors and derivation failures
- Language ecosystem skills: `golang-ecosystem`, `rust-ecosystem`, `haskell-ecosystem` for packaging conventions
