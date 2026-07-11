---
name: Nix Ecosystem
description: This skill should be used when the user asks to "write nix", "nix expression", "flake.nix", "home-manager config", "programs.*", "services.*", "nixpkgs packaging", "buildGoModule", "buildRustPackage", or works with Nix language, flakes, or Home Manager. Provides comprehensive Nix ecosystem patterns and best practices.
version: 2.1.0
---

<purpose>
  Provide comprehensive patterns for Nix language, flakes, and Home Manager configuration.
</purpose>

<nix_language>
  <fundamentals>
    <concept name="lazy_evaluation">
      <description>Nix is lazily evaluated. Expressions are only computed when needed.</description>
      <example>
        <note>Only evaluates needed attributes</note>
        let
        expensive = builtins.trace "Computing expensive" (1 + 1);
        in
        { a = 1; b = expensive; }.a # Does not compute expensive
      </example>
    </concept>

    <concept name="pure_functions">
      <description>All Nix functions are pure. Same inputs always produce same outputs.</description>
      <example>
        <note>Pure function - always returns same result for same input</note>
        double = x: x * 2;

        <note>Avoid side effects; use derivations for build actions</note>
        buildResult = pkgs.stdenv.mkDerivation { ... };
      </example>
    </concept>

    <concept name="attribute_sets">
      <description>Primary data structure in Nix</description>
      <example>
        <note>Basic attribute set</note>
        { attr1 = value1; attr2 = value2; }

        <note>Access patterns</note>
        set.attr
        set."attr-with-dashes"

        <note>Recursive attribute set</note>
        rec { a = 1; b = a + 1; }
      </example>
    </concept>
  </fundamentals>

  <patterns>
    <pattern name="let_in">
      <description>Local bindings for complex expressions</description>
      <example>
        let
        helper = x: x + 1;
        value = helper 5;
        in
        value * 2
      </example>
    </pattern>

    <pattern name="with">
      <description>Bring attribute set into scope</description>
      <example>
        with pkgs; [ git vim tmux ]
      </example>
      <warning>Avoid nested with; prefer explicit references for clarity</warning>
    </pattern>

    <pattern name="inherit">
      <description>Copy attributes from another set</description>
      <example>
        { inherit (pkgs) git vim; inherit name version; }
      </example>
    </pattern>

    <pattern name="overlay">
      <description>Modify or extend nixpkgs</description>
      <example>
        final: prev: {
        myPackage = prev.myPackage.override { ... };
        }
      </example>
      <decision_tree name="when_to_use">
        <question>Do you need to modify existing packages or add new ones globally?</question>
        <if_yes>Use overlays to extend nixpkgs</if_yes>
        <if_no>Use local package definitions with callPackage</if_no>
      </decision_tree>
    </pattern>

    <pattern name="callPackage">
      <description>Dependency injection pattern</description>
      <example>
        myPackage = pkgs.callPackage ./package.nix { };
      </example>
    </pattern>

    <pattern name="mkDerivation">
      <description>Standard package builder</description>
      <example>
        pkgs.stdenv.mkDerivation {
        pname = "mypackage";
        version = "1.0.0";
        src = fetchFromGitHub { ... };

        nativeBuildInputs = [ pkgs.cmake ];
        buildInputs = [ pkgs.openssl ];

        installPhase = ''
        mkdir -p $out/bin
        cp mypackage $out/bin/
        '';
        }
      </example>
      <note>Required attributes: pname, version, src</note>
      <note>Standard phases: unpackPhase, patchPhase, configurePhase, buildPhase, installPhase</note>
    </pattern>

    <pattern name="build_inputs">
      <description>Dependency specification in derivations</description>
      <example>
        {
        # Tools run at build time (compilers, build tools)
        nativeBuildInputs = [ cmake pkg-config ];

        # Libraries linked at runtime
        buildInputs = [ openssl zlib ];
        }
      </example>
    </pattern>

    <pattern name="options_config">
      <description>NixOS/Home Manager module structure</description>
      <example>
        { config, lib, pkgs, ... }:
        {
        options.myModule = {
        enable = lib.mkEnableOption "my module";
        setting = lib.mkOption {
        type = lib.types.str;
        default = "value";
        description = "A setting";
        };
        };

        config = lib.mkIf config.myModule.enable { # configuration when enabled
        };
        }
      </example>
    </pattern>

    <pattern name="mkOption">
      <description>Define module options with types and defaults</description>
      <example>
        options.myOption = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable my feature";
        example = true;
        };
      </example>
      <note>Common types: lib.types.bool, lib.types.str, lib.types.listOf, lib.types.attrsOf</note>
    </pattern>

    <pattern name="mkEnableOption">
      <description>Shorthand for boolean enable option</description>
      <example>
        enable = lib.mkEnableOption "my service";
      </example>
    </pattern>

    <pattern name="mkPackageOption">
      <description>Option for specifying a package with a default from pkgs</description>
      <example>
        package = lib.mkPackageOption pkgs "mypackage" { };
        # With nullable:
        package = lib.mkPackageOption pkgs "mypackage" { nullable = true; };
      </example>
    </pattern>

    <pattern name="mkMerge">
      <description>Merge multiple configuration sets</description>
      <example>
        config = lib.mkMerge [
        (lib.mkIf config.services.foo.enable { environment.systemPackages = [ pkgs.foo ]; })
        (lib.mkIf config.services.bar.enable { environment.systemPackages = [ pkgs.bar ]; })
        ];
      </example>
    </pattern>

    <pattern name="mkForce">
      <description>Override a value with higher priority (force it over other definitions)</description>
      <example>
        services.openssh.settings.PermitRootLogin = lib.mkForce "no";
      </example>
      <warning>Use sparingly; prefer lib.mkDefault for setting lower-priority defaults instead</warning>
    </pattern>
  </patterns>

  <anti_patterns>
    <avoid name="impure_paths">
      <description>Directly referencing absolute paths breaks reproducibility</description>
      <instead>Use fetchurl, fetchFromGitHub, or relative paths within the repository</instead>
    </avoid>

    <avoid name="nested_with">
      <description>Multiple nested with statements reduce code clarity</description>
      <instead>Prefer explicit attribute access (pkgs.git) for better readability</instead>
    </avoid>

    <avoid name="rec_overuse">
      <description>Recursive attribute sets can be hard to understand and maintain</description>
      <instead>Use let-in for complex recursive definitions</instead>
    </avoid>

    <avoid name="string_interpolation_abuse">
      <description>Using string interpolation for path operations is error-prone</description>
      <instead>Use lib functions for path manipulation (lib.concatStringsSep, builtins.path)</instead>
    </avoid>

    <avoid name="with_lib_extensively">
      <description>Using "with lib;" at the top of modules pollutes scope and hides where functions come from</description>
      <instead>Use explicit attribute access: lib.mkIf, lib.mkOption, lib.types.str. Only use "with" for narrow scopes like package lists (with pkgs; [ ... ])</instead>
    </avoid>

    <avoid name="fetchurl_without_hash">
      <description>Using fetchurl or fetchTarball without a hash (sha256/hash) breaks reproducibility</description>
      <instead>Always provide hash or sha256. Use nix-prefetch-url or lib.fakeHash to obtain the correct hash</instead>
    </avoid>

    <avoid name="legacy_nix_env">
      <description>Using nix-env -i for imperative package management creates mutable unreproducible state</description>
      <instead>Use declarative configuration via flakes, home.packages, or environment.systemPackages</instead>
    </avoid>

    <avoid name="mutable_nix_var_state">
      <description>Relying on mutable state in /nix/var (e.g., nix-channel, nix-env profiles) breaks reproducibility</description>
      <instead>Use flake inputs for pinning and lock files for reproducibility</instead>
    </avoid>
  </anti_patterns>
</nix_language>

<flakes>
  <note>Flakes are the de facto standard for Nix projects. While technically still behind an experimental flag in upstream Nix, they are universally adopted in the ecosystem. Lix (a community fork of the Nix evaluator) also supports flakes.</note>

  <concept name="flake_structure">
    <description>Basic structure of a flake.nix file</description>
    <example>
      {
      description = "Project description";

      inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      };

      outputs = { self, nixpkgs, ... }@inputs: { # output attributes
      };
      }
    </example>
  </concept>

  <outputs>
    <concept name="packages">
      <description>Derivations for nix build</description>
      <example>
        packages.x86_64-linux.default = pkgs.hello;
      </example>
    </concept>

    <concept name="devShells">
      <description>Development environments for nix develop</description>
      <example>
        devShells.x86_64-linux.default = pkgs.mkShell {
        packages = [ pkgs.nodejs ];
        };
      </example>
    </concept>

    <concept name="apps">
      <description>Runnable applications for nix run</description>
      <example>
        apps.x86_64-linux.default = {
        type = "app";
        program = "${pkgs.hello}/bin/hello";
        };
      </example>
    </concept>

    <concept name="overlays">
      <description>Nixpkgs overlays</description>
      <example>
        overlays.default = final: prev: {
        myPackage = prev.callPackage ./myPackage.nix { };
        };
      </example>
    </concept>

    <concept name="nixosModules">
      <description>NixOS modules</description>
      <example>
        nixosModules.default = { config, lib, pkgs, ... }: {
        options.services.myService = { ... };
        config = { ... };
        };
      </example>
    </concept>

    <concept name="homeManagerModules">
      <description>Home Manager modules</description>
      <example>
        homeManagerModules.default = { config, lib, pkgs, ... }: {
        options.programs.myProgram = { ... };
        config = { ... };
        };
      </example>
    </concept>

    <concept name="nixosConfigurations">
      <description>Full NixOS system configurations</description>
      <example>
        nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./configuration.nix ];
        };
      </example>
    </concept>

    <concept name="homeConfigurations">
      <description>Home Manager configurations</description>
      <example>
        homeConfigurations."user@host" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [ ./home.nix ];
        };
      </example>
    </concept>
  </outputs>

  <patterns>
    <pattern name="github_input">
      <description>Reference GitHub repositories as flake inputs</description>
      <example>
        inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        # Specific branch
        stable.url = "github:NixOS/nixpkgs/nixos-25.11";
        # Specific revision
        pinned.url = "github:owner/repo/abc123def";
        };
      </example>
    </pattern>

    <pattern name="follows">
      <description>Share input between flakes to avoid duplication</description>
      <example>
        home-manager = {
        url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
        };
      </example>
    </pattern>

    <pattern name="flake_false">
      <description>Non-flake inputs for legacy repositories</description>
      <example>
        my-source = {
        url = "github:owner/repo";
        flake = false;
        };
      </example>
    </pattern>

    <pattern name="per_system">
      <description>Generate outputs for multiple systems</description>
      <example>
        outputs = { self, nixpkgs, ... }:
        let
        systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
        forAllSystems = nixpkgs.lib.genAttrs systems;
        in {
        packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in { default = pkgs.hello; }
        );
        };
      </example>
      <decision_tree name="when_to_use">
        <question>Does your flake need to support multiple platforms?</question>
        <if_yes>Use per-system pattern with genAttrs or flake-utils</if_yes>
        <if_no>Define outputs for single system directly</if_no>
      </decision_tree>
    </pattern>

    <pattern name="devShell">
      <description>Development environment with packages and hooks</description>
      <example>
        devShells.default = pkgs.mkShell {
        packages = with pkgs; [ nodejs yarn ];
        shellHook = ''
        echo "Development environment ready"
        '';
        };
      </example>
    </pattern>
  </patterns>

  <tools>
    <tool name="nix flake update">
      <description>Update all inputs to their latest versions</description>
      <use_case>Regular dependency updates</use_case>
    </tool>

    <tool name="nix flake update input-name">
      <description>Update a specific input</description>
      <use_case>Selective updates to test compatibility</use_case>
    </tool>

    <tool name="nix flake show">
      <description>Display all flake outputs</description>
      <use_case>Exploring available packages and configurations</use_case>
    </tool>

    <tool name="nix flake check">
      <description>Validate flake and run checks</description>
      <use_case>CI/CD validation, pre-commit checks</use_case>
    </tool>
  </tools>

  <tooling>
    <tool name="formatter">
      <description>nixfmt (RFC-style) is the standard Nix formatter, replacing nixpkgs-fmt</description>
      <example>nix fmt # uses formatter defined in flake.nix</example>
      <flake_config>formatter.x86_64-linux = pkgs.nixfmt-rfc-style;</flake_config>
    </tool>

    <tool name="lsp">
      <description>Language servers for Nix: nil (nix-community) and nixd (nix-community) are the two main options</description>
      <note>nil is lightweight and widely used; nixd provides richer nixpkgs-aware completions</note>
    </tool>

    <tool name="nix-direnv">
      <description>Fast direnv integration for Nix flake devShells, caches environments to avoid slow re-evaluation</description>
      <example>
        programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        };
      </example>
    </tool>
  </tooling>

  <ecosystem_note>
    <description>Lix is a community fork of the Nix evaluator with improved error messages and performance. It is a drop-in replacement for CppNix and supports flakes. Be aware of its existence when users reference it.</description>
  </ecosystem_note>
</flakes>

<home_manager>
  <concept name="module_structure">
    <description>Standard Home Manager module structure</description>
    <example>
      { config, pkgs, lib, ... }:
      {
      options.custom.feature = {
      enable = lib.mkEnableOption "feature description";
      };

      config = lib.mkIf config.custom.feature.enable { # configuration when enabled
      };
      }
    </example>
  </concept>

  <patterns>
    <pattern name="by_program">
      <description>Organize modules by program name</description>
      <example>
        home-manager/programs/git.nix
        home-manager/programs/neovim.nix
        home-manager/programs/tmux.nix
      </example>
    </pattern>

    <pattern name="by_category">
      <description>Organize modules by category</description>
      <example>
        home-manager/development/default.nix
        home-manager/shell/default.nix
        home-manager/editors/default.nix
      </example>
    </pattern>

    <pattern name="imports">
      <description>Import multiple modules</description>
      <example>
        imports = [
        ./programs/git.nix
        ./programs/neovim.nix
        ./shell/fish.nix
        ];
      </example>
    </pattern>

    <pattern name="basic_enable">
      <description>Enable a program with defaults</description>
      <example>
        programs.git.enable = true;
      </example>
    </pattern>

    <pattern name="with_options">
      <description>Enable and configure a program</description>
      <example>
        programs.git = {
        enable = true;
        userName = "name";
        userEmail = "email";
        extraConfig = {
        core.editor = "nvim";
        init.defaultBranch = "main";
        };
        };
      </example>
      <decision_tree name="when_to_use">
        <question>Does Home Manager provide built-in module for this program?</question>
        <if_yes>Use programs.* with configuration options</if_yes>
        <if_no>Use home.file or xdg.configFile for manual configuration</if_no>
      </decision_tree>
    </pattern>

    <pattern name="package_override">
      <description>Use alternative package version</description>
      <example>
        programs.git = {
        enable = true;
        package = pkgs.gitFull;
        };
      </example>
    </pattern>

    <pattern name="home.file">
      <description>Manage dotfiles directly</description>
      <example>
        home.file.".config/app/config" = {
        source = ./config;
        # or
        text = ''
        key = value
        '';
        };
      </example>
    </pattern>

    <pattern name="xdg.configFile">
      <description>XDG config directory files</description>
      <example>
        xdg.configFile."app/config".source = ./config;
      </example>
    </pattern>

    <pattern name="home.sessionVariables">
      <description>Environment variables for login shells</description>
      <example>
        home.sessionVariables = {
        EDITOR = "nvim";
        PAGER = "less";
        };
      </example>
    </pattern>

    <pattern name="home.sessionPath">
      <description>Add directories to PATH</description>
      <example>
        home.sessionPath = [ "$HOME/.local/bin" ];
      </example>
    </pattern>
  </patterns>

  <common_modules>
    <concept name="programs.git">
      <description>Git version control configuration</description>
      <example>
        programs.git = {
        enable = true;
        userName = "Your Name";
        userEmail = "email@example.com";
        signing = {
        key = "KEY_ID";
        signByDefault = true;
        };
        aliases = {
        co = "checkout";
        st = "status";
        };
        extraConfig = {
        core.editor = "nvim";
        };
        };
      </example>
    </concept>

    <concept name="programs.neovim">
      <description>Neovim editor configuration</description>
      <example>
        programs.neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
        plugins = with pkgs.vimPlugins; [
        vim-commentary
        vim-surround
        ];
        extraConfig = ''
        set number
        set relativenumber
        '';
        extraLuaConfig = ''
        vim.opt.expandtab = true
        '';
        };
      </example>
    </concept>

    <concept name="programs.fish">
      <description>Fish shell configuration</description>
      <example>
        programs.fish = {
        enable = true;
        shellInit = ''
        set -g fish_greeting
        '';
        shellAliases = {
        ll = "ls -lah";
        };
        functions = {
        gitignore = "curl -sL https://www.gitignore.io/api/$argv";
        };
        plugins = [
        { name = "z"; src = pkgs.fishPlugins.z.src; }
        ];
        };
      </example>
    </concept>

    <concept name="programs.tmux">
      <description>Terminal multiplexer configuration</description>
      <example>
        programs.tmux = {
        enable = true;
        terminal = "screen-256color";
        keyMode = "vi";
        plugins = with pkgs.tmuxPlugins; [
        sensible
        yank
        ];
        extraConfig = ''
        set -g mouse on
        '';
        };
      </example>
    </concept>

    <concept name="programs.direnv">
      <description>Directory-specific environment loader</description>
      <example>
        programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
        };
      </example>
    </concept>
  </common_modules>

  <best_practices>
    <practice priority="critical">
      Use programs.* when available instead of manual configuration
    </practice>

    <practice priority="critical">
      Set home.stateVersion to your initial HM version and do not change after initial setup unless migrating
    </practice>

    <practice priority="high">
      Group related configurations in separate modules for maintainability
    </practice>

    <practice priority="high">
      Use lib.mkIf for conditional configuration
    </practice>

    <practice priority="medium">
      Prefer xdg.configFile over home.file for XDG-compliant apps
    </practice>

    <practice priority="medium">
      Use home.packages for additional packages not configured via programs.*
    </practice>
  </best_practices>

  <concept name="state_version">
    <description>Track Home Manager state version for compatibility</description>
    <example>
      home.stateVersion = "25.11"; # Stable baseline for 2026-05 configurations.
    </example>
    <warning>Do not change after initial setup unless migrating</warning>
  </concept>

  <concept name="minimal_mode">
    <description>HM 25.05+ supports minimal mode for faster evaluation</description>
    <example>
      imports = [
      "${modulesPath}/programs/fzf.nix"
      ];
    </example>
    <note>Advanced users optimizing evaluation time</note>
  </concept>
</home_manager>

<nixos>
  <patterns>
    <pattern name="basic">
      <description>Basic NixOS configuration with Home Manager</description>
      <example>
        nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        ];
        specialArgs = { inherit inputs; };
        };
      </example>
    </pattern>

    <pattern name="standalone_home_manager">
      <description>Standalone Home Manager without NixOS</description>
      <example>
        homeConfigurations."user@host" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [ ./home.nix ];
        extraSpecialArgs = { inherit inputs; };
        };
      </example>
    </pattern>

    <pattern name="as_nixos_module">
      <description>Home Manager as a NixOS module</description>
      <example>
        {
        imports = [ home-manager.nixosModules.home-manager ];

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.username = import ./home.nix;
        }
      </example>
    </pattern>
  </patterns>
</nixos>

<nixpkgs_packaging>
  <concept name="languages_frameworks">
    <description>Nixpkgs provides language-specific packaging infrastructure documented in doc/languages-frameworks/. Use Context7 with library ID /nixos/nixpkgs to retrieve up-to-date packaging patterns for each language.</description>
  </concept>

  <context7_topics>
    <topic name="go">
      <query>buildGoModule</query>
      <query_alt>go packaging</query_alt>
      <builders>buildGoModule</builders>
      <language_version>Go 1.26</language_version>
      <related_skill>golang-ecosystem</related_skill>
    </topic>

    <topic name="rust">
      <query>rustPlatform.buildRustPackage</query>
      <query_alt>rust packaging</query_alt>
      <builders>rustPlatform.buildRustPackage, rustPlatform.importCargoLock</builders>
      <language_version>Rust edition 2024</language_version>
      <related_skill>rust-ecosystem</related_skill>
    </topic>

    <topic name="haskell">
      <query>haskellPackages</query>
      <query_alt>haskell packaging</query_alt>
      <builders>haskellPackages.mkDerivation, haskellPackages.callCabal2nix</builders>
      <language_version>GHC 9.14</language_version>
      <related_skill>haskell-ecosystem</related_skill>
    </topic>

    <topic name="php">
      <query>buildComposerProject2</query>
      <query_alt>php packaging</query_alt>
      <builders>php.buildComposerProject2</builders>
      <language_version>PHP 8.5</language_version>
      <related_skill>php-ecosystem</related_skill>
    </topic>

    <topic name="swift">
      <query>swift packaging</query>
      <builders>stdenv.mkDerivation with swift and swiftpm as nativeBuildInputs</builders>
      <language_version>Swift 6.3</language_version>
      <related_skill>swift-ecosystem</related_skill>
    </topic>

    <topic name="c_cpp">
      <query>cmake</query>
      <query_alt>meson</query_alt>
      <builders>stdenv.mkDerivation with cmake or meson as nativeBuildInputs</builders>
      <related_skill>c-ecosystem, cplusplus-ecosystem</related_skill>
    </topic>

    <topic name="nodejs">
      <query>node packaging</query>
      <query_alt>buildNpmPackage</query_alt>
      <builders>buildNpmPackage, buildYarnPackage</builders>
      <language_version>Node.js 24 LTS</language_version>
      <related_skill>typescript-ecosystem</related_skill>
    </topic>

    <topic name="python">
      <query>python packaging</query>
      <query_alt>buildPythonPackage</query_alt>
      <builders>python3Packages.buildPythonPackage, python3Packages.buildPythonApplication</builders>
      <language_version>Python 3.13</language_version>
    </topic>

    <topic name="common_lisp">
      <query>lisp packaging</query>
      <builders>sbcl.buildASDFSystem, lispPackages_new.sbclPackages</builders>
      <related_skill>common-lisp-ecosystem</related_skill>
    </topic>
  </context7_topics>

  <decision_tree name="packaging_approach">
    <question>Is the target language listed in context7_topics above?</question>
    <if_yes>Use the corresponding Context7 topic query with library ID /nixos/nixpkgs</if_yes>
    <if_no>Use get-library-docs with context7CompatibleLibraryID="/nixos/nixpkgs" and topic="LANGUAGE packaging" as a fallback</if_no>
  </decision_tree>

  <best_practices>
    <practice priority="critical">
      Always consult Context7 for the latest nixpkgs packaging patterns before writing language-specific derivations
    </practice>
    <practice priority="high">
      Use language-specific builders (buildGoModule, rustPlatform.buildRustPackage, etc.) instead of raw mkDerivation
    </practice>
    <practice priority="medium">
      Cross-reference with the corresponding ecosystem skill for language-specific conventions
    </practice>
  </best_practices>
</nixpkgs_packaging>

<devenv>
  <note>devenv 2.0 is the current version. See devenv-ecosystem skill for detailed patterns.</note>
</devenv>

<flake_input_discipline>
  <principle name="single_dependency_graph">
    <description>
      Every flake input can itself declare inputs, and by default each resolves independently. A `follows` declaration rewires a transitive input to point at a node higher in the graph (usually the root), collapsing what would be several independent copies into one shared instance. The goal is a single coherent dependency graph: one nixpkgs, one lib, one set of core libraries for the whole closure.
    </description>
    <mechanism>
      Without follows, `flake.lock` gains multiple nixpkgs nodes (nixpkgs, nixpkgs_2, nixpkgs_3, ...), each pinned to a different revision. Downstream this produces divergent versions of core libraries (glibc, openssl, libgit2), binary-cache misses that force rebuilds from source, and occasional ABI mismatches between components that were meant to interoperate.
    </mechanism>
    <example>
      <note>Collapse a dependency's nixpkgs (and its own transitive inputs) onto the root</note>
      home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      };

      <note>Deeper override: force a grandchild input to the root as well</note>
      some-tool.inputs.helper.inputs.nixpkgs.follows = "nixpkgs";
    </example>
  </principle>

  <principle name="justified_exceptions">
    <description>
      A follows exception is sometimes correct and should not be reflexively "fixed":
      a component that is incompatible with nixpkgs-unstable can be pointed at a
      stable channel instead (inputs.nixpkgs.follows = "nixpkgs-stable"), and a tool
      whose prebuilt closure lives in its own binary cache may need to keep its own
      nixpkgs during initial bootstrap so the cache actually hits (add follows only
      after the first successful build).
    </description>
    <rule>Keep each exception a single isolated node, and record the reason inline with a link to the upstream issue that justifies it. An undocumented divergence is the failure mode, not the divergence itself.</rule>
    <symptom>If you see the same package built twice with different hashes, or an unexpected rebuild-from-source after a flake update, inspect the lock for duplicated nixpkgs nodes and audit which inputs are missing a follows.</symptom>
  </principle>
</flake_input_discipline>

<home_manager_module_design>
  <pattern name="directory_autodiscovery">
    <description>
      When a module deploys many files that share one directory structure (skill files, command files, per-tool configs), enumerate them with `builtins.readDir` instead of listing each by hand. Adding a file then needs no edit to the module.
    </description>
    <example>
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
    </example>
    <note>`builtins.readDir` returns an attrset of name to type ("directory" | "regular" | "symlink"); filter by type rather than assuming all entries are files.</note>
    <warning>Flakes only see git-tracked files, so a newly created directory or file is invisible to evaluation until it is `git add`ed. "Works with `nix build` locally but the file is missing after switch" is almost always an un-added path.</warning>
  </pattern>

  <pattern name="modules_as_flake_output_ssot">
    <description>
      Export reusable Home Manager modules as `homeManagerModules.*` flake outputs so several configurations consume one source of truth instead of vendoring copies. Downstream flakes import the modules and can reference the upstream flake's own inputs (inputs.upstream.inputs.someTool) so pins stay centrally managed.
    </description>
    <warning name="double_import_of_ambient_module_args">
      If a bundle module internally imports a companion that sets `_module.args.foo`, importing that same companion AGAIN alongside the bundle in the same scope throws `error: attribute '...foo' is defined multiple times`. The module system does not dedupe two imports that both define a `_module.args` entry. Rely on the ambient one the bundle already provides; do not import it a second time. (This surfaces at build time, not always at eval time.)
    </warning>
    <note>Modules pulled in via `imports = [ ./x ]` must take `...` in their signature so the module system can pass extra arguments (lib, the distributed specialArgs, etc.).</note>
  </pattern>

  <pattern name="specialargs_constant_distribution">
    <description>
      Distribute cross-cutting constants and inputs to every module through `specialArgs` (nixosSystem / as a NixOS module) or `extraSpecialArgs` (standalone homeManagerConfiguration) rather than relative imports. Modules then consume them as ordinary function arguments.
    </description>
    <example>
      <note>Producer</note>
      extraSpecialArgs = { inherit inputs; inherit (lib) constants; };

      <note>Consumer</note>
      { constants, inputs, ... }: { nix.settings = constants.nixSettings; }
    </example>
    <note>For a value that only some modules need, `_module.args.foo = ...;` sets it locally without threading it through every signature.</note>
  </pattern>

  <pattern name="two_layer_git_hooks">
    <description>
      Git hooks compose cleanly in two layers because git config scoping is local &gt; global. A global layer (Home Manager `programs.git.hooks`, setting `core.hooksPath`) applies to every repository; a per-project layer (for example devenv setting `git config --local core.hooksPath .git/hooks`) overrides it inside that project. The two coexist without conflict.
    </description>
    <note>In hook shell scripts, use NUL-delimited iteration (`-z` / `-0` / `--null`) for safe filename handling and prefer `printf` over `echo` for robustness against filenames and options.</note>
  </pattern>
</home_manager_module_design>

<packaging_patterns>
  <concept name="rust_current_pattern">
    <description>
      Package Rust with `rustPlatform.buildRustPackage`, providing a `cargoHash` computed over the vendored crate sources.
    </description>
    <example>
      rustPlatform.buildRustPackage (finalAttrs: {
      pname = "tool";
      version = "1.2.3";

      src = fetchFromGitHub {
      owner = "owner"; repo = "repo";
      rev = "refs/tags/v${finalAttrs.version}";
      hash = "sha256-...";
      };

      cargoHash = "sha256-...";

      nativeBuildInputs = [ pkg-config cmake ];
      buildInputs = [ openssl ]
      ++ lib.optionals stdenv.hostPlatform.isDarwin [ apple-sdk_15 ];

      doCheck = false; # when the suite needs network/tooling absent in the sandbox
      })
    </example>
    <note name="fetchCargoVendor_is_default">On nixpkgs 25.05 and later the fetchCargoVendor mechanism is the default and non-optional; a bare `useFetchCargoVendor = true;` is now redundant and nixpkgs emits a message asking you to remove it. Older derivations set it explicitly — drop it when you touch them. `cargoHash` is still required (or use `cargoLock` for a path-based lockfile).</note>
    <note name="hash_bootstrap">Seed unknown hashes with `lib.fakeHash`, build, then copy the real hash from the mismatch error. `cmake` is commonly needed transitively (e.g. `aws-lc-sys` via rustls).</note>
    <warning name="override_cargoDeps">`overrideAttrs` cannot change vendored dependencies by setting a new `cargoHash` — it has no effect after the fact. To override for a version bump you must override the resulting `cargoDeps` and set its `outputHash`.</warning>
  </concept>

  <concept name="darwin_sdk_pattern">
    <description>
      Nixpkgs replaced the per-framework Darwin inputs with a bundled, versioned SDK. Drop unversioned `darwin.apple_sdk.frameworks.*` entries entirely — the default SDK is in the Darwin stdenv now — and add `apple-sdk_NN` (for example `apple-sdk_15`) to `buildInputs` only when a specific SDK version is required. The SDK propagates libiconv/libresolv automatically, so conditionally adding those on Darwin is generally no longer necessary. The legacy `darwin.apple_sdk_11_0.*` compatibility stubs have been removed and now error out.
    </description>
  </concept>

  <decision_tree name="js_builder_selection">
    <question>Which JavaScript/TypeScript builder fits the project?</question>
    <branch condition="npm lockfile, no build step">buildNpmPackage with npmDepsHash and dontNpmBuild = true</branch>
    <branch condition="npm lockfile, needs build">buildNpmPackage with npmDepsHash; use finalAttrs when src needs the version</branch>
    <branch condition="pnpm lockfile">stdenvNoCC.mkDerivation + fetchPnpmDeps (hash + fetcherVersion) + pnpmConfigHook; the pnpm major used to fetch deps must match the one used to build</branch>
    <branch condition="bun lockfile">two-phase: a fixed-output derivation runs `bun install --frozen-lockfile` for node_modules, then the main build consumes it</branch>
    <branch condition="turbo/monorepo orchestrator">see build_cache_daemon_sandbox below — the daemon must be disabled</branch>
    <note>Use `nodejs-slim_NN` for the runtime wrapper and full `nodejs_NN` for the build; wrap node entrypoints with `makeBinaryWrapper`; install app trees under `$out/lib/&lt;pname&gt;/`.</note>
  </decision_tree>

  <anti_pattern name="build_cache_daemon_sandbox">
    <description>
      Monorepo build tools that spawn a background daemon holding file locks on cache directories (turbo, and similar) break Nix sandbox teardown with errors like `cannot unlink "...drv-0": Directory not empty`, because the daemon is still alive and its cache tree is locked when the sandbox tries to clean up.
    </description>
    <instead>
      Disable the daemon by both environment variable and CLI flag, force cache bypass, and remove the cache directories at the end of buildPhase:
      export TURBO_DAEMON=0; export TURBO_FORCE=true; pnpm turbo build --no-daemon; then `rm -rf .turbo node_modules/.cache/turbo || true`. The general rule: any build helper that persists a daemon or on-disk cache across invocations must be forced into a single-shot, cache-clean mode inside the Nix sandbox.
    </instead>
  </anti_pattern>

  <best_practices>
    <practice priority="high">Compute all hashes as SRI (sha256-...); language builders carry a second hash for the dependency set (cargoHash, vendorHash, npmDepsHash, pnpmDeps.hash) that must be regenerated whenever the lockfile changes.</practice>
    <practice priority="medium">Prefer language-specific builders over raw mkDerivation, and keep `meta` complete (description, homepage, license, maintainers, platforms, mainProgram for CLIs).</practice>
  </best_practices>
</packaging_patterns>

<secrets_outside_store>
  <principle name="store_is_world_readable">
    <description>
      The Nix store is world-readable by design (store paths are 0555 directories and 0444 files) and content-addressed. Anything materialized into the store — via `writeText`, `builtins.toJSON` piped into a store file, an unquoted path literal, or string-interpolating a path — becomes readable by every local user AND is copied into any binary cache the closure is pushed to. Therefore no plaintext OR ciphertext secret should ever enter the store.
    </description>
  </principle>

  <anti_pattern name="path_literal_store_capture">
    <description>
      A path literal, or a string produced by coercing a path (`"${./secrets.yaml}"`, `"${inputs.self}/secrets.yaml"`), is copied into the store as an evaluation input. Even a store-resident sops file is then in the closure.
    </description>
    <instead>
      Reference the secret by a runtime string path that is never a Nix path type — an absolute string like `"/var/lib/app-secrets/secrets.yaml"` that resolves only on the target at runtime. With sops-nix this also requires `sops.validateSopsFiles = false;`, because the validator asserts `builtins.isPath sopsFile` and rejects a plain string. The effect: the encrypted file is provisioned out-of-band on the target and is never a store input.
    </instead>
  </anti_pattern>

  <pattern name="runtime_derived_key">
    <description>
      Keep decryption key material out of source and store by deriving it at boot. A oneshot systemd service (Type = oneshot, RemainAfterExit = true, ordered before the units that need it) derives an age key from the host SSH key with `ssh-to-age`, writes it 0600, and decrypted secrets land under a runtime tmpfs (for example /run/secrets) — all outside the store. Make the derivation idempotent (skip if a valid key already exists) and order it explicitly after host-key generation.
    </description>
  </pattern>

  <verification name="closure_leak_audit">
    <description>
      Prove no secret entered the closure by querying its requisites and confirming the secret path is absent:
      nix-store -q --requisites &lt;drvPath-or-outPath&gt; | grep -i secrets
      An empty result is the pass condition. Do this whenever a config touches secret handling, and especially before pushing a closure to a public cache.
    </description>
  </verification>

  <note>Declarative session/config modules that serialize their whole config into the store (writeText of a toJSON blob) turn any `env`, `vars`, command strings, or hook bodies into world-readable store data. Treat such modules as non-secret-only unless reworked to avoid store materialization.</note>
</secrets_outside_store>

<darwin_linux_builder>
  <note>Apple Silicon macOS cannot natively build *-linux derivations. To build them locally you register a Linux builder VM as a nix build machine; for anything large you offload to native CI and only substitute the result.</note>

  <decision_tree name="builder_mechanism_spectrum">
    <question>How much native performance versus simplicity does the Linux builder need?</question>
    <branch condition="simplest, zero extra inputs">nix-darwin's built-in `nix.linux-builder` (QEMU-backed). It auto-configures buildMachines, distributedBuilds, and builders-use-substitutes. Slowest execution but least to maintain.</branch>
    <branch condition="faster boot / native virtio">a MicroVM framework (microvm.nix) or a lightweight vfkit-based runner over Apple's hypervisor. On macOS vfkit is the practical hypervisor (built-in virtiofs, no 9p and no TAP); microvm.nix on Darwin needs a compatible pin because virtiofsd is unavailable on some revisions, and requires `storeOnDisk = false` plus `vmHostPackages = nixpkgs.legacyPackages.aarch64-darwin`. Faster and more native I/O, more moving parts, and thinner community precedent on Darwin.</branch>
    <branch condition="near-native speed">an Apple Virtualization.framework based builder VM. Direct framework access (no QEMU overhead) and Rosetta gives x86_64-linux at roughly 70-90% native speed versus QEMU's order-of-magnitude slowdown. Most native, most bespoke.</branch>
    <note>Only one linux-builder mechanism can be active at once — they are mutually exclusive on `nix.buildMachines`/`nix.linux-builder`.</note>
  </decision_tree>

  <constraints name="native_framework_path">
    <constraint>Networking is NAT/user-mode only: no TAP interface and no port forwarding. The guest IP is not fixed — discover it via the ARP table (by a deterministic guest MAC) or the macOS DHCP lease file (`/var/db/dhcpd_leases`, matched by a fixed guest hostname).</constraint>
    <constraint>`/nix/store` is shared read-only over virtiofs with an overlay providing the writable layer; these backends do not support 9p, and `storeOnDisk` is often false (ephemeral store in the overlay).</constraint>
    <constraint>No nested virtualization on Apple Silicon: the builder VM will not run inside another VM (fails in typical CI M-series runners).</constraint>
  </constraints>

  <principle name="key_handling">
    <description>
      Generate the builder SSH keypair at activation time and keep the private key on the host; never commit it (the config repo may be public). Share only the public key into the guest over a read-only mount and inject it into authorized_keys at boot. Bootstrap may use a password, but harden to key-only (PermitRootLogin prohibit-password, PasswordAuthentication no) once the builder works.
    </description>
  </principle>

  <concept name="cross_platform_substitution">
    <description>
      Building or substituting foreign-platform derivations from Darwin needs explicit flags, because NixOS `system.build.toplevel` sets preferLocalBuild (allowSubstitutes = false) and the daemon negatively caches narinfo 404s.
    </description>
    <example>
      nix build .#packages.aarch64-linux.&lt;pkg&gt; \
      --max-jobs 0 \
      --option extra-platforms aarch64-linux \
      --option always-allow-substitutes true
      <note>Add `--option narinfo-cache-negative-ttl 0` when a stale negative cache hides a now-available path.</note>
    </example>
    <note>Permanent form in nix-darwin: `nix.settings = { extra-platforms = [ "aarch64-linux" ]; always-allow-substitutes = true; };`. A closure often spans multiple substituters (cache.nixos.org plus a project cache), so both must be configured for the fetch to complete.</note>
  </concept>

  <principle name="offload_large_builds_to_ci">
    <description>
      For large closures (hundreds to thousands of derivations) build on native CI (a real aarch64-linux runner) and push to a binary cache; let the Mac and its builder VM only substitute the finished closure and activate. Building a big *-linux closure on a virtiofs-overlay store can churn the store hard enough to lose store-path visibility mid-build — an architectural limit of the shared/overlay store, not a transient bug.
    </description>
  </principle>

  <operational name="launchd_notes">
    <note>Run the builder (and comparable long-lived agents) as a launchd job in the correct domain. A daemon that needs a graphical session must live in the `gui` domain; leaving it in the background/user domain can inject `LimitLoadToSessionType = Background` and cause flaky bootstrap where a process is alive but `launchctl print gui/$UID/...` shows nothing loaded. During migration from a previous user-domain job, explicitly boot out the old `user/$UID/...` job or launchd may respawn it.</note>
    <note>Raise file-descriptor limits in layers or you hit "Too many open files": a boot-time daemon for `kern.maxfiles` / `kern.maxfilesperproc` / `launchctl limit maxfiles`, per-agent `NumberOfFiles` (soft and hard), and — the commonly missed one — the GUI/user session's own `launchctl limit maxfiles`.</note>
    <note>On-demand socket activation (launchd listens on the build port and starts the VM only when a build connects, with a TTL idle-shutdown) keeps an idle builder from consuming resources.</note>
  </operational>
</darwin_linux_builder>

<deployment_operations>
  <concept name="switch_to_configuration_lock">
    <description>
      NixOS activation (`switch-to-configuration`, as driven by deploy-rs) takes a non-blocking exclusive flock at `/run/nixos/switch-to-configuration.lock`. A second concurrent activation fails immediately (no retry, exit code 11 / EAGAIN) rather than queueing.
    </description>
    <recovery>
      A local Ctrl+C on deploy-rs can leave the remote `switch-to-configuration` running and holding the lock; kill the leftover remote process. A crash while holding the lock leaves a stale lock file to remove manually. Check with `fuser` / `ps` on the target before assuming the lock is stale.
    </recovery>
  </concept>

  <concept name="rollback_and_timeout_tuning">
    <description>
      deploy-rs `magicRollback` verifies post-activation reachability and rolls back on failure, but when activation starts many or slow services (for example a fleet of containers) startup can exceed the verification window and trigger a false rollback. In that case disable `magicRollback` and rely on `autoRollback`, and raise `activationTimeout` (e.g. 600s) to fit the real startup time.
    </description>
    <example>
      { autoRollback = true; magicRollback = false; activationTimeout = 600; }
    </example>
    <note>Keep ephemeral containers' `systemd` `TimeoutStopSec` low (e.g. 15s) so a slow shutdown does not hold the activation lock; with the default 90s across many containers, teardown alone can block the next deploy for a long time.</note>
  </concept>

  <note name="nixos_containers">Declared NixOS containers are auto-started by systemd on activation; a full reset of systemd-machined state before applying can be needed to avoid EEXIST races on redeploy. Unrelated per-tool gotcha: tmux `base-index 1` means new windows must be appended with `-a` or they collide on the in-use index.</note>
</deployment_operations>

<flake_parts_ci>
  <concept name="standard_rust_flake_composition">
    <description>
      A common reproducible-build stack: flake-parts (perSystem for multi-system outputs) + crane (Rust builds) + treefmt-nix (formatting) + rust-overlay (toolchain). crane's two-phase build compiles dependencies once (`buildDepsOnly`) and caches them separately from the package build (`buildPackage`), so source-only changes do not rebuild the dependency graph. The treefmt-nix flakeModule auto-adds a `checks.treefmt` and the `formatter` output.
    </description>
    <example>
      <note>Typical checks surface</note>
      checks.clippy = ...; # cargo clippy -- -D warnings
      checks.tests  = ...; # cargo test
      # checks.treefmt is contributed automatically by the treefmt flakeModule
    </example>
    <note>Version coupling is the recurring maintenance cost: tool versions must match between the vendored lockfile and nixpkgs (wasm-bindgen-cli vs the wasm-bindgen pin in Cargo.lock; the pnpm major used to fetch deps vs to build). A dependency-set hash such as `pnpmDeps.hash` must be regenerated whenever its lockfile changes, or the build fails on a hash mismatch.</note>
  </concept>

  <concept name="ci_verification_chain">
    <description>
      A representative CI chain: actionlint (validate the workflow itself) then `nix flake check` (evaluate + run all checks) then `nix build` of the real outputs, pushing results to a binary cache so downstream jobs (e.g. a docs/site deploy, or a deploy target) only substitute. Gating on `nix flake check` before build catches evaluation and formatting regressions cheaply.
    </description>
  </concept>

  <principle name="sandbox_test_discipline">
    <description>
      The Nix build sandbox has no network access and a minimal toolset (no git), and Nix builds Rust in the release profile. Two consequences must be encoded in the tests:
    </description>
    <rule>Tests needing network or git must be `#[ignore]`d (or feature-gated) so the sandboxed `cargo test` still passes; run them outside the sandbox in a dev shell.</rule>
    <rule>The release profile strips `debug_assert!`, so a `#[should_panic]` test that asserts a `debug_assert!` fires must be gated with `#[cfg(debug_assertions)]`, or it fails under the Nix build even though it passes under a debug `cargo test`.</rule>
  </principle>
</flake_parts_ci>

<workflow>
  <phase name="analyze">
    <objective>Understand Nix expression requirements</objective>
    <step order="1">
  <action>1. Identify target: flake, module, derivation, or expression</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Check existing patterns in project</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Consult Serena memories for conventions</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="implement">
    <objective>Write idiomatic Nix code</objective>
    <step order="1">
  <action>1. Follow patterns from decision trees</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Use appropriate lib functions</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Apply best practices for target type</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
  <phase name="validate">
    <objective>Verify Nix expression correctness</objective>
    <step order="1">
  <action>1. Check syntax with nix flake check</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>2. Verify evaluation with nix eval</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
    <step order="1">
  <action>3. Test build with nix build</action>
  <tool>Workflow guidance</tool>
  <output>Step completed</output>
</step>
  </phase>
</workflow>

<rules priority="critical">
  <rule>Always verify flake.lock is updated after any input change</rule>
  <rule>Prefer `pkgs.lib` functions over inline Nix expressions for complex transformations</rule>
  <rule>Use `mkIf` and `mkMerge` for conditional module options, not plain `if` at top level</rule>
</rules>
<rules priority="standard">
  <rule>Check nixpkgs-unstable before nixpkgs-stable for newer package versions</rule>
  <rule>Use `nix flake check` to validate flake outputs before committing</rule>
  <rule>Document non-obvious overlay and override rationale in comments</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Style inconsistency in Nix expression</example>
    <example severity="medium">Evaluation error or type mismatch</example>
    <example severity="high">Build failure in derivation</example>
    <example severity="critical">Impure expression breaking reproducibility</example>
  </examples>
</error_escalation>

<constraints>
  <must>Use lib functions for complex operations</must>
  <must>Follow project's existing Nix patterns</must>
  <must>Maintain reproducibility in all expressions</must>
  <avoid>Impure paths and absolute references</avoid>
  <avoid>Nested with statements</avoid>
  <avoid>Overusing rec for attribute sets</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Symbol operations for navigating Nix expressions and module definitions</skill>
  <skill name="context7-usage">Fetch latest nixpkgs and Home Manager documentation</skill>
  <skill name="investigation-patterns">Debug evaluation errors and understand derivation failures</skill>
  <skill name="ecosystem skills">Language-specific conventions for nixpkgs packaging via golang-ecosystem, rust-ecosystem, haskell-ecosystem, php-ecosystem, swift-ecosystem, c-ecosystem, cplusplus-ecosystem, common-lisp-ecosystem</skill>
  <skill name="devenv-ecosystem">Devenv 2.0 configuration patterns</skill>
</related_skills>
<related_agents>
  <agent name="explore">Locate code patterns and references in this skill domain</agent>
  <agent name="quality-assurance">Review implementation quality against this skill guidance</agent>
  <agent name="code-quality">Analyze code complexity and suggest refactoring improvements</agent>
</related_agents>
