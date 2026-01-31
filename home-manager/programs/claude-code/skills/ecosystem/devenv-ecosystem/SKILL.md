---
name: Devenv Ecosystem
description: This skill should be used when the user asks to "devenv", "devenv.nix", "languages.*", "services.*", "git-hooks", "devenv shell", "devenv up", "devenv build", or works with devenv development environments. Provides comprehensive devenv configuration patterns.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for devenv configuration, focusing on language setup, services, tooling (git-hooks, scripts, processes), outputs, and profiles.
</purpose>

<scope>
  <focus>devenv.nix configuration files and devenv-specific options</focus>
  <defer_to skill="nix-ecosystem">
    Nix language syntax and semantics, flake structure and inputs, general NixOS module patterns, lib.* functions
  </defer_to>
  <unique_coverage>
    languages.* language modules, services.* service configuration, git-hooks.hooks pre-commit hooks, processes and process-compose, scripts and tasks, outputs and profiles
  </unique_coverage>
</scope>

<tools>
  <tool name="devenv init">Initialize new devenv project with devenv.nix and .envrc</tool>
  <tool name="devenv shell">Enter development shell with all configured tools</tool>
  <tool name="devenv up">Start all configured processes (process-compose)</tool>
  <tool name="devenv build">Build configured outputs</tool>
  <tool name="devenv test">Run configured tests</tool>
  <tool name="devenv tasks run">Execute configured tasks</tool>
  <tool name="devenv update">Update devenv inputs</tool>
  <tool name="devenv gc">Garbage collect unused derivations</tool>
  <tool name="devenv info">Show environment information</tool>
  <tool name="devenv container">Build OCI container images</tool>
</tools>

<concepts>
  <concept name="devenv_nix">
    <description>Main configuration file for devenv environments</description>
    <example>
      { pkgs, config, ... }:
      {
        languages.python.enable = true;
        packages = [ pkgs.git ];
        services.postgres.enable = true;
      }
    </example>
  </concept>

  <concept name="module_system">
    <description>Devenv uses the NixOS module system for configuration</description>
    <example>
      { pkgs, lib, config, ... }:
      {
        options.myOption = lib.mkEnableOption "custom option";
        config = lib.mkIf config.myOption {
          packages = [ pkgs.hello ];
        };
      }
    </example>
  </concept>

  <concept name="inputs">
    <description>External flake inputs available in devenv.nix</description>
    <example>
      { pkgs, inputs, ... }:
      {
        packages = [ inputs.my-flake.packages.${pkgs.system}.default ];
      }
    </example>
  </concept>
</concepts>

<languages>
  <common_patterns>
    <pattern name="basic_enable">
      <description>Enable language support with defaults</description>
      <example>
        languages.python.enable = true;
      </example>
    </pattern>

    <pattern name="version_specification">
      <description>Specify language version (available for some languages)</description>
      <example>
        languages.python.version = "3.11.3";
        languages.ruby.version = "3.2.1";
        languages.rust.channel = "stable";
      </example>
    </pattern>

    <pattern name="package_override">
      <description>Use custom package instead of default</description>
      <example>
        languages.go.package = pkgs.go_1_21;
        languages.java.jdk.package = pkgs.jdk17;
      </example>
    </pattern>

    <pattern name="version_file">
      <description>Read version from file (Ruby, Python)</description>
      <example>
        languages.ruby.versionFile = "./.ruby-version";
        languages.python.versionFile = "./.python-version";
      </example>
    </pattern>

    <pattern name="dev_tooling">
      <description>Configure LSP, debugger, linter, formatter (devenv 1.8+)</description>
      <example>
        languages.rust.dev = {
          lsp.enable = true;
          debugger.enable = true;
          linter.enable = true;
          formatter.enable = true;
        };
      </example>
    </pattern>
  </common_patterns>

  <category name="systems">
    <description>System programming languages (C, C++, Rust, Go, Zig, Nim, Odin, V)</description>

    <language name="rust">
      <example>
        languages.rust = {
          enable = true;
          channel = "stable"; # or "nightly", "beta"
          components = [ "rustc" "cargo" "clippy" "rustfmt" "rust-analyzer" ];
          targets = [ "wasm32-unknown-unknown" ];
        };
      </example>
      <output_pattern>
        outputs.rust-app = config.languages.rust.import ./rust-app {};
      </output_pattern>
    </language>

    <language name="go">
      <example>
        languages.go = {
          enable = true;
          package = pkgs.go_1_22;
        };
      </example>
      <output_pattern>
        outputs.go-app = config.languages.go.import ./go-app {};
      </output_pattern>
    </language>

    <language name="c_cpp">
      <example>
        languages.c.enable = true;
        languages.cplusplus.enable = true;
      </example>
    </language>

    <language name="zig">
      <example>
        languages.zig = {
          enable = true;
          package = pkgs.zig;
          zls.package = pkgs.zls;
        };
      </example>
    </language>

    <language name="nim">
      <example>
        languages.nim = {
          enable = true;
          package = pkgs.nim;
        };
      </example>
    </language>

    <language name="odin">
      <example>
        languages.odin = {
          enable = true;
          package = pkgs.odin;
        };
      </example>
    </language>

    <language name="v">
      <example>
        languages.v = {
          enable = true;
          package = pkgs.vlang;
        };
      </example>
    </language>
  </category>

  <category name="interpreted">
    <description>Interpreted languages (Python, Ruby, PHP, Perl, Lua)</description>

    <language name="python">
      <example>
        languages.python = {
          enable = true;
          version = "3.12";
          uv.enable = true;
          uv.sync.enable = true;
        };
      </example>
      <poetry_example>
        languages.python = {
          enable = true;
          poetry = {
            enable = true;
            install.enable = true;
            install.allExtras = true;
          };
        };
      </poetry_example>
      <output_pattern>
        outputs.python-app = config.languages.python.import ./python-app {};
      </output_pattern>
    </language>

    <language name="ruby">
      <example>
        languages.ruby = {
          enable = true;
          version = "3.3.0";
          bundler.enable = true;
        };
      </example>
    </language>

    <language name="php">
      <example>
        languages.php = {
          enable = true;
          package = pkgs.php83;
          extensions = [ "opcache" "redis" ];
        };
      </example>
    </language>

    <language name="perl">
      <example>
        languages.perl.enable = true;
      </example>
    </language>

    <language name="lua">
      <example>
        languages.lua.enable = true;
      </example>
    </language>
  </category>

  <category name="jvm">
    <description>JVM languages (Java, Kotlin, Scala, Clojure)</description>

    <language name="java">
      <example>
        languages.java = {
          enable = true;
          jdk.package = pkgs.jdk21;
          maven.enable = true;
          gradle.enable = true;
        };
      </example>
    </language>

    <language name="kotlin">
      <example>
        languages.kotlin.enable = true;
      </example>
    </language>

    <language name="scala">
      <example>
        languages.scala = {
          enable = true;
          package = pkgs.scala_3;
          sbt.enable = true;
        };
      </example>
    </language>

    <language name="clojure">
      <example>
        languages.clojure.enable = true;
      </example>
    </language>
  </category>

  <category name="functional">
    <description>Functional languages (Haskell, OCaml, Elixir, Erlang, Elm, PureScript)</description>

    <language name="haskell">
      <example>
        languages.haskell = {
          enable = true;
          languageServer = pkgs.haskell-language-server;
          stack = {
            enable = true;
            args = [ "--no-nix" "--system-ghc" "--no-install-ghc" ];
          };
        };
      </example>
    </language>

    <language name="ocaml">
      <example>
        languages.ocaml = {
          enable = true;
          packages = pkgs.ocaml-ng.ocamlPackages_5_1;
        };
      </example>
    </language>

    <language name="elixir">
      <example>
        languages.elixir.enable = true;
      </example>
    </language>

    <language name="erlang">
      <example>
        languages.erlang.enable = true;
      </example>
    </language>

    <language name="elm">
      <example>
        languages.elm.enable = true;
      </example>
    </language>

    <language name="purescript">
      <example>
        languages.purescript = {
          enable = true;
          package = pkgs.purescript;
        };
      </example>
    </language>

    <language name="gleam">
      <example>
        languages.gleam.enable = true;
      </example>
    </language>

    <language name="idris">
      <example>
        languages.idris = {
          enable = true;
          package = pkgs.idris2;
        };
      </example>
    </language>

    <language name="lean4">
      <example>
        languages.lean4 = {
          enable = true;
          package = pkgs.lean4;
        };
      </example>
    </language>

    <language name="unison">
      <example>
        languages.unison.enable = true;
      </example>
    </language>
  </category>

  <category name="web">
    <description>Web development (JavaScript, TypeScript, Deno)</description>

    <language name="javascript">
      <example>
        languages.javascript = {
          enable = true;
          package = pkgs.nodejs_22;
          npm.enable = true;
          npm.install.enable = true;
        };
      </example>
      <pnpm_example>
        languages.javascript = {
          enable = true;
          pnpm.enable = true;
          pnpm.install.enable = true;
        };
      </pnpm_example>
      <bun_example>
        languages.javascript = {
          enable = true;
          bun.enable = true;
          bun.install.enable = true;
        };
      </bun_example>
      <yarn_example>
        languages.javascript = {
          enable = true;
          yarn.enable = true;
          yarn.install.enable = true;
        };
      </yarn_example>
      <corepack_example>
        languages.javascript = {
          enable = true;
          corepack.enable = true;
        };
      </corepack_example>
    </language>

    <language name="typescript">
      <example>
        languages.typescript.enable = true;
      </example>
    </language>

    <language name="deno">
      <example>
        languages.deno.enable = true;
      </example>
    </language>
  </category>

  <category name="other">
    <description>Other languages</description>

    <language name="nix">
      <example>
        languages.nix = {
          enable = true;
          lsp.package = pkgs.nil; # or pkgs.nixd
        };
      </example>
    </language>

    <language name="swift">
      <example>
        languages.swift.enable = true;
      </example>
    </language>

    <language name="crystal">
      <example>
        languages.crystal.enable = true;
      </example>
    </language>

    <language name="dart">
      <example>
        languages.dart.enable = true;
      </example>
    </language>

    <language name="r">
      <example>
        languages.r.enable = true;
      </example>
    </language>

    <language name="julia">
      <example>
        languages.julia.enable = true;
      </example>
    </language>

    <language name="fortran">
      <example>
        languages.fortran.enable = true;
      </example>
    </language>

    <language name="pascal">
      <example>
        languages.pascal = {
          enable = true;
          lazarus.enable = true;
        };
      </example>
    </language>

    <language name="raku">
      <example>
        languages.raku.enable = true;
      </example>
    </language>

    <language name="racket">
      <example>
        languages.racket.enable = true;
      </example>
    </language>

    <language name="standardml">
      <example>
        languages.standardml.enable = true;
      </example>
    </language>

    <language name="solidity">
      <example>
        languages.solidity = {
          enable = true;
          package = pkgs.solc;
        };
      </example>
    </language>

    <language name="terraform">
      <example>
        languages.terraform.enable = true;
      </example>
    </language>

    <language name="opentofu">
      <example>
        languages.opentofu.enable = true;
      </example>
    </language>

    <language name="ansible">
      <example>
        languages.ansible = {
          enable = true;
          package = pkgs.ansible;
        };
      </example>
    </language>

    <language name="helm">
      <example>
        languages.helm = {
          enable = true;
          languageServer.enable = true;
        };
      </example>
    </language>

    <language name="cue">
      <example>
        languages.cue = {
          enable = true;
          package = pkgs.cue;
        };
      </example>
    </language>

    <language name="jsonnet">
      <example>
        languages.jsonnet.enable = true;
      </example>
    </language>

    <language name="typst">
      <example>
        languages.typst = {
          enable = true;
          fontPaths = [ "${pkgs.roboto}/share/fonts/truetype" ];
        };
      </example>
    </language>

    <language name="texlive">
      <example>
        languages.texlive.enable = true;
      </example>
    </language>

    <language name="gawk">
      <example>
        languages.gawk.enable = true;
      </example>
    </language>

    <language name="shell">
      <example>
        languages.shell.enable = true;
      </example>
    </language>

    <language name="robotframework">
      <example>
        languages.robotframework.enable = true;
      </example>
    </language>

    <language name="vala">
      <example>
        languages.vala.enable = true;
      </example>
    </language>

    <language name="dotnet">
      <example>
        languages.dotnet.enable = true;
      </example>
    </language>
  </category>

  <decision_tree name="language_selection">
    <question>What type of project are you building?</question>
    <branch condition="System/performance-critical">Use Rust, Go, Zig, or C/C++</branch>
    <branch condition="Web backend">Use JavaScript/TypeScript, Python, Ruby, Go, or Rust</branch>
    <branch condition="Web frontend">Use JavaScript/TypeScript with npm/pnpm/bun</branch>
    <branch condition="Data science/ML">Use Python with uv or poetry</branch>
    <branch condition="Functional programming">Use Haskell, OCaml, Elixir, or Gleam</branch>
    <branch condition="JVM enterprise">Use Java, Kotlin, or Scala with gradle/maven/sbt</branch>
    <branch condition="Infrastructure">Use Terraform, OpenTofu, Ansible, or Nix</branch>
  </decision_tree>

  <decision_tree name="package_manager_selection">
    <question>Which JavaScript/TypeScript package manager to use?</question>
    <branch condition="Existing project">Match existing lock file (package-lock.json=npm, pnpm-lock.yaml=pnpm, yarn.lock=yarn, bun.lockb=bun)</branch>
    <branch condition="New project needing speed">Use pnpm or bun</branch>
    <branch condition="Maximum compatibility">Use npm (most widely supported)</branch>
    <branch condition="Monorepo workspace">Use pnpm or yarn with workspaces</branch>
    <branch condition="Strict version control">Use corepack.enable with packageManager in package.json</branch>
  </decision_tree>

  <decision_tree name="devenv_vs_flakes">
    <question>Should you use devenv or pure Nix flakes?</question>
    <branch condition="Need services (databases, caches)">Use devenv - services.* is simpler</branch>
    <branch condition="Need pre-commit hooks">Use devenv - git-hooks integration built-in</branch>
    <branch condition="Need process supervision">Use devenv - process-compose integration</branch>
    <branch condition="Need language version management">Use devenv - languages.* handles versions</branch>
    <branch condition="Pure Nix derivations only">Use flakes - devenv adds overhead</branch>
    <branch condition="NixOS system configuration">Use flakes - devenv is for dev environments</branch>
  </decision_tree>
</languages>

<services>
  <description>Background services for development environments</description>

  <category name="databases">
    <service name="postgres">
      <example>
        services.postgres = {
          enable = true;
          package = pkgs.postgresql_16;
          initialDatabases = [{ name = "myapp"; }];
          listen_addresses = "127.0.0.1";
          port = 5432;
          extensions = [ "postgis" "pgvector" ];
          settings = {
            max_connections = 100;
          };
        };
      </example>
    </service>

    <service name="mysql">
      <example>
        services.mysql = {
          enable = true;
          package = pkgs.mysql80;
          initialDatabases = [{ name = "myapp"; }];
        };
      </example>
    </service>

    <service name="mongodb">
      <example>
        services.mongodb = {
          enable = true;
        };
      </example>
    </service>

    <service name="redis">
      <example>
        services.redis = {
          enable = true;
          port = 6379;
        };
      </example>
    </service>

    <service name="memcached">
      <example>
        services.memcached = {
          enable = true;
        };
      </example>
    </service>

    <service name="minio">
      <example>
        services.minio = {
          enable = true;
        };
      </example>
    </service>

    <service name="elasticsearch">
      <example>
        services.elasticsearch = {
          enable = true;
        };
      </example>
    </service>

    <service name="opensearch">
      <example>
        services.opensearch = {
          enable = true;
        };
      </example>
    </service>
  </category>

  <category name="messaging">
    <service name="rabbitmq">
      <example>
        services.rabbitmq = {
          enable = true;
        };
      </example>
    </service>

    <service name="kafka">
      <example>
        services.kafka = {
          enable = true;
        };
      </example>
    </service>
  </category>

  <category name="web">
    <service name="nginx">
      <example>
        services.nginx = {
          enable = true;
          httpConfig = ''
            server {
              listen 8080;
              location / {
                proxy_pass http://localhost:3000;
              }
            }
          '';
        };
      </example>
    </service>

    <service name="caddy">
      <example>
        services.caddy = {
          enable = true;
        };
      </example>
    </service>

    <service name="varnish">
      <example>
        services.varnish = {
          enable = true;
          vcl = ''
            vcl 4.0;
            backend default {
              .host = "127.0.0.1";
              .port = "8080";
            }
          '';
        };
      </example>
    </service>
  </category>

  <category name="mail">
    <service name="mailhog">
      <example>
        services.mailhog.enable = true;
      </example>
    </service>

    <service name="mailpit">
      <example>
        services.mailpit.enable = true;
      </example>
    </service>
  </category>

  <decision_tree name="service_selection">
    <question>What type of storage/service do you need?</question>
    <branch condition="Relational data">Use postgres or mysql</branch>
    <branch condition="Document storage">Use mongodb</branch>
    <branch condition="Caching">Use redis or memcached</branch>
    <branch condition="Object storage">Use minio</branch>
    <branch condition="Search">Use elasticsearch or opensearch</branch>
    <branch condition="Message queue">Use rabbitmq or kafka</branch>
    <branch condition="Reverse proxy">Use nginx or caddy</branch>
    <branch condition="Email testing">Use mailhog or mailpit</branch>
  </decision_tree>
</services>

<tooling>
  <category name="git_hooks">
    <description>Pre-commit hooks via git-hooks.hooks</description>
    <example>
      git-hooks.hooks = {
        # Formatters
        nixfmt-rfc-style.enable = true;
        prettier.enable = true;
        black.enable = true;
        rustfmt.enable = true;

        # Linters
        eslint.enable = true;
        clippy.enable = true;
        shellcheck.enable = true;

        # Security
        detect-private-keys.enable = true;
        check-merge-conflicts.enable = true;

        # Git hygiene
        check-case-conflicts.enable = true;
        editorconfig-checker.enable = true;
      };
    </example>

    <custom_hook>
      git-hooks.hooks.my-custom-hook = {
        enable = true;
        name = "my-custom-hook";
        entry = "${pkgs.bash}/bin/bash -c 'echo Running custom hook'";
        files = "\\.nix$";
        pass_filenames = true;
      };
    </custom_hook>

    <common_hooks>
      <hook name="nixfmt-rfc-style">Nix code formatter (RFC style)</hook>
      <hook name="prettier">JavaScript/TypeScript/CSS/HTML formatter</hook>
      <hook name="black">Python code formatter</hook>
      <hook name="rustfmt">Rust code formatter</hook>
      <hook name="ormolu">Haskell code formatter</hook>
      <hook name="gofmt">Go code formatter</hook>
      <hook name="eslint">JavaScript/TypeScript linter</hook>
      <hook name="clippy">Rust linter</hook>
      <hook name="shellcheck">Shell script linter</hook>
      <hook name="yamllint">YAML linter</hook>
      <hook name="actionlint">GitHub Actions linter</hook>
      <hook name="hadolint">Dockerfile linter</hook>
      <hook name="markdownlint">Markdown linter</hook>
      <hook name="detect-private-keys">Prevent committing private keys</hook>
      <hook name="check-merge-conflicts">Prevent committing merge conflicts</hook>
      <hook name="check-case-conflicts">Detect case conflicts in filenames</hook>
      <hook name="editorconfig-checker">Check editorconfig compliance</hook>
      <hook name="treefmt">Universal formatter via treefmt</hook>
    </common_hooks>
  </category>

  <category name="scripts">
    <description>Custom scripts available in devenv shell</description>
    <example>
      scripts = {
        build.exec = "cargo build --release";
        test.exec = "cargo test";
        lint.exec = "cargo clippy -- -D warnings";
        fmt.exec = "cargo fmt";
        dev.exec = "cargo watch -x run";

        # With description
        deploy = {
          exec = "kubectl apply -f k8s/";
          description = "Deploy to Kubernetes";
        };

        # Using packages
        migrate = {
          exec = "${pkgs.dbmate}/bin/dbmate up";
          description = "Run database migrations";
        };
      };
    </example>
  </category>

  <category name="processes">
    <description>Background processes via process-compose</description>
    <example>
      processes = {
        web.exec = "npm run dev";
        api.exec = "cargo run";
        worker.exec = "python worker.py";
      };
    </example>

    <advanced_example>
      processes = {
        web = {
          exec = "npm run dev";
          process-compose = {
            depends_on.api.condition = "process_healthy";
            readiness_probe = {
              http_get = {
                host = "localhost";
                port = 3000;
              };
            };
          };
        };
        api = {
          exec = "cargo run";
          process-compose = {
            readiness_probe = {
              http_get = {
                host = "localhost";
                port = 8080;
                path = "/health";
              };
            };
          };
        };
      };
    </advanced_example>
  </category>

  <category name="tasks">
    <description>Named tasks with dependencies (devenv 1.7+)</description>
    <example>
      tasks = {
        "build:frontend" = {
          exec = "npm run build";
          before = [ "devenv:enterShell" ];
        };
        "build:backend" = {
          exec = "cargo build --release";
          before = [ "devenv:enterShell" ];
        };
        "build:all" = {
          exec = "echo Build complete";
          after = [ "build:frontend" "build:backend" ];
        };
      };
    </example>
  </category>

  <category name="env">
    <description>Environment variables</description>
    <example>
      env = {
        DATABASE_URL = "postgres://localhost/myapp";
        REDIS_URL = "redis://localhost:6379";
        SECRET_KEY = "development-secret";
        DEBUG = "true";
      };
    </example>

    <dotenv_example>
      dotenv.enable = true;
      dotenv.filename = ".env.local";
    </dotenv_example>
  </category>

  <category name="enterShell">
    <description>Commands to run when entering shell</description>
    <example>
      enterShell = ''
        echo "Welcome to the development environment!"
        export PS1="(devenv) $PS1"
      '';
    </example>
  </category>

  <category name="enterTest">
    <description>Commands to run in test environment</description>
    <example>
      enterTest = ''
        echo "Running tests..."
        cargo test
      '';
    </example>
  </category>
</tooling>

<outputs>
  <description>Build outputs and packaging (devenv 1.6+)</description>

  <pattern name="language_import">
    <description>Package applications using language-specific builders</description>
    <example>
      { config, ... }:
      {
        languages.rust.enable = true;
        languages.python.enable = true;
        languages.go.enable = true;

        outputs = {
          rust-app = config.languages.rust.import ./rust-app {};
          python-app = config.languages.python.import ./python-app {};
          go-app = config.languages.go.import ./go-app {};
        };
      }
    </example>
  </pattern>

  <pattern name="custom_derivation">
    <description>Custom derivation as output</description>
    <example>
      { pkgs, ... }:
      {
        outputs.my-app = pkgs.stdenv.mkDerivation {
          pname = "my-app";
          version = "1.0.0";
          src = ./.;
          buildPhase = "make";
          installPhase = "make install PREFIX=$out";
        };
      }
    </example>
  </pattern>
</outputs>

<profiles>
  <description>Configuration profiles for different contexts (devenv 1.9+)</description>

  <pattern name="basic_profile">
    <description>Define profile-specific configuration</description>
    <example>
      { pkgs, ... }:
      {
        languages.python.enable = true;
        languages.python.version = "3.12";

        profiles = {
          "python-3.11".config = {
            languages.python.version = "3.11";
          };
          "python-3.10".config = {
            languages.python.version = "3.10";
          };
        };
      }
    </example>
    <usage>devenv shell --profile python-3.11</usage>
  </pattern>

  <pattern name="team_profile">
    <description>Profiles based on team roles</description>
    <example>
      { pkgs, ... }:
      {
        languages.nix.enable = true;

        profiles = {
          backend.module = {
            languages.rust.enable = true;
            services.postgres.enable = true;
            services.redis.enable = true;
          };

          frontend.module = {
            languages.javascript.enable = true;
            languages.typescript.enable = true;
          };

          fullstack.extends = [ "backend" "frontend" ];
        };
      }
    </example>
  </pattern>

  <pattern name="host_user_profile">
    <description>Automatic profiles based on hostname or user</description>
    <example>
      { pkgs, ... }:
      {
        profiles = {
          hostname."ci-server".module = {
            env.CI = "true";
            packages = [ pkgs.buildkit ];
          };

          user."developer".module = {
            packages = [ pkgs.gh ];
          };
        };
      }
    </example>
  </pattern>
</profiles>

<patterns>
  <pattern name="minimal_config">
    <description>Minimal devenv.nix for quick setup</description>
    <example>
      { pkgs, ... }:
      {
        packages = [ pkgs.git ];
        languages.python.enable = true;
      }
    </example>
  </pattern>

  <pattern name="full_stack">
    <description>Full-stack web development setup</description>
    <example>
      { pkgs, config, ... }:
      {
        packages = [ pkgs.git pkgs.curl pkgs.jq ];

        languages = {
          javascript = {
            enable = true;
            package = pkgs.nodejs_22;
            pnpm.enable = true;
          };
          typescript.enable = true;
        };

        services = {
          postgres = {
            enable = true;
            initialDatabases = [{ name = "app_dev"; }];
          };
          redis.enable = true;
        };

        processes = {
          frontend.exec = "pnpm dev";
          api.exec = "pnpm api:dev";
        };

        scripts = {
          db-migrate.exec = "pnpm db:migrate";
          db-seed.exec = "pnpm db:seed";
        };

        git-hooks.hooks = {
          prettier.enable = true;
          eslint.enable = true;
        };
      }
    </example>
  </pattern>

  <pattern name="nix_project">
    <description>Nix development project</description>
    <example>
      { pkgs, ... }:
      {
        languages.nix = {
          enable = true;
          lsp.package = pkgs.nixd;
        };

        packages = [ pkgs.nixfmt-rfc-style ];

        git-hooks.hooks = {
          nixfmt-rfc-style.enable = true;
          editorconfig-checker.enable = true;
        };
      }
    </example>
  </pattern>

  <pattern name="mcp_integration">
    <description>MCP server integration (devenv 1.7+)</description>
    <example>
      { pkgs, inputs, ... }:
      let
        mcp-servers = inputs.mcp-servers-nix.lib.evalModule pkgs {
          programs.nixos.enable = true;
        };
      in {
        files.".mcp.json".json = {
          mcpServers = mcp-servers.config.settings.servers;
        };
      }
    </example>
  </pattern>

  <decision_tree name="configuration_approach">
    <question>What type of project are you setting up?</question>
    <branch condition="Quick prototype">Use minimal_config pattern</branch>
    <branch condition="Web application">Use full_stack pattern</branch>
    <branch condition="Nix development">Use nix_project pattern</branch>
    <branch condition="Multi-team project">Use profiles with team modules</branch>
  </decision_tree>
</patterns>

<anti_patterns>
  <avoid name="hardcoded_versions">
    <description>Hardcoding specific package versions instead of using version options</description>
    <instead>Use languages.*.version or languages.*.package with nixpkgs packages</instead>
  </avoid>

  <avoid name="shell_commands_in_packages">
    <description>Using shell commands to install packages instead of using packages attribute</description>
    <instead>Use packages = [ pkgs.package-name ]; for declarative package management</instead>
  </avoid>

  <avoid name="impure_enterShell">
    <description>Using network-dependent commands in enterShell</description>
    <instead>Use tasks or scripts for network operations; keep enterShell fast and offline-capable</instead>
  </avoid>

  <avoid name="excessive_enterShell">
    <description>Running slow operations in enterShell</description>
    <instead>Use tasks with explicit triggers instead of always running in enterShell</instead>
  </avoid>

  <avoid name="mixing_package_managers">
    <description>Enabling multiple conflicting package managers (npm and pnpm and yarn)</description>
    <instead>Choose one package manager per project (npm OR pnpm OR yarn OR bun)</instead>
  </avoid>

  <avoid name="secrets_in_env">
    <description>Storing production secrets in env attribute</description>
    <instead>Use dotenv.enable with .env files in .gitignore, or use external secret management</instead>
  </avoid>
</anti_patterns>

<workflow>
  <phase name="analyze">
    <objective>Understand devenv configuration requirements</objective>
    <step>1. Identify required languages and their versions</step>
    <step>2. Determine needed services (databases, caches, etc.)</step>
    <step>3. Plan background processes and scripts</step>
    <step>4. Consider team profiles if multi-developer project</step>
  </phase>
  <phase name="implement">
    <objective>Write devenv.nix configuration</objective>
    <step>1. Start with minimal configuration (packages, primary language)</step>
    <step>2. Add services with appropriate settings</step>
    <step>3. Configure git-hooks for code quality</step>
    <step>4. Add scripts for common operations</step>
    <step>5. Set up processes for development servers</step>
    <step>6. Add profiles if needed</step>
  </phase>
  <phase name="validate">
    <objective>Verify devenv configuration works</objective>
    <step>1. Run devenv shell to verify environment</step>
    <step>2. Run devenv up to verify services start</step>
    <step>3. Test git hooks with sample commits</step>
    <step>4. Verify scripts work as expected</step>
  </phase>
</workflow>

<best_practices>
  <practice priority="critical">
    Use languages.*.enable instead of adding language packages directly to packages list
  </practice>

  <practice priority="critical">
    Keep enterShell fast; move slow operations to tasks or scripts
  </practice>

  <practice priority="high">
    Use git-hooks for consistent code quality across team
  </practice>

  <practice priority="high">
    Use services.* instead of running databases manually
  </practice>

  <practice priority="high">
    Use dotenv.enable for environment-specific configuration
  </practice>

  <practice priority="medium">
    Document scripts with description attribute
  </practice>

  <practice priority="medium">
    Use profiles for multi-context development (different language versions, team roles)
  </practice>

  <practice priority="medium">
    Use outputs for packaging applications when needed
  </practice>
</best_practices>

<rules priority="critical">
  <rule>Use devenv languages.* options instead of manually adding language packages</rule>
  <rule>Use devenv services.* instead of manual service setup</rule>
  <rule>Keep enterShell lightweight and fast</rule>
</rules>

<rules priority="standard">
  <rule>Enable git-hooks for code quality enforcement</rule>
  <rule>Use scripts for common development commands</rule>
  <rule>Document configuration with comments</rule>
  <rule>Use profiles for environment variations</rule>
</rules>

<context7_integration>
  <library_id>/cachix/devenv</library_id>
  <trust_score>9.7</trust_score>
  <snippets>1354</snippets>
  <usage_patterns>
    <pattern topic="languages">Fetch specific language configuration options</pattern>
    <pattern topic="services">Fetch service configuration details</pattern>
    <pattern topic="git-hooks">Fetch available pre-commit hooks</pattern>
    <pattern topic="outputs">Fetch packaging patterns</pattern>
    <pattern topic="profiles">Fetch profile configuration (1.9+)</pattern>
  </usage_patterns>
</context7_integration>

<error_escalation>
  <level severity="low">
    <example>Missing optional configuration attribute</example>
    <action>Note in output, suggest improvement</action>
  </level>
  <level severity="medium">
    <example>Service configuration incomplete or enterShell too slow</example>
    <action>Document issue, suggest refactoring approach</action>
  </level>
  <level severity="high">
    <example>Configuration fails to evaluate or services fail to start</example>
    <action>Debug with devenv info, present options to user</action>
  </level>
  <level severity="critical">
    <example>Security issue in configuration (secrets exposed, unsafe permissions)</example>
    <action>Block operation, require explicit user acknowledgment</action>
  </level>
</error_escalation>

<related_agents>
  <agent name="explore">Finding existing devenv configurations and patterns</agent>
  <agent name="design">Evaluating devenv architecture and service dependencies</agent>
  <agent name="security">Reviewing devenv configuration for security issues</agent>
</related_agents>

<related_skills>
  <skill name="nix-ecosystem">Nix language fundamentals, flakes, Home Manager (devenv uses Nix)</skill>
  <skill name="serena-usage">Symbol operations for navigating devenv configurations</skill>
  <skill name="context7-usage">Fetch latest devenv documentation</skill>
</related_skills>

<constraints>
  <must>Use devenv languages.* options for language configuration</must>
  <must>Use devenv services.* for service management</must>
  <must>Follow project existing devenv patterns if present</must>
  <avoid>Hardcoding versions instead of using version options</avoid>
  <avoid>Slow operations in enterShell</avoid>
  <avoid>Mixing multiple conflicting package managers</avoid>
  <avoid>Storing secrets in env attribute</avoid>
</constraints>
