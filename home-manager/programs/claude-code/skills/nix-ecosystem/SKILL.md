---
name: Nix Ecosystem
description: This skill should be used when the user asks to "write nix", "nix expression", "flake.nix", "home-manager config", "programs.*", "services.*", or works with Nix language, flakes, or Home Manager. Provides comprehensive Nix ecosystem patterns and best practices.
version: 0.1.0
---

<purpose>
Provide comprehensive patterns for Nix language, flakes, and Home Manager configuration.
</purpose>

<nix_language>
<fundamentals>
<concept name="lazy_evaluation">
<description>Nix is lazily evaluated. Expressions are only computed when needed.</description>
<use>Conditional includes and optional dependencies</use>
</concept>

<concept name="pure_functions">
<description>All Nix functions are pure. Same inputs always produce same outputs.</description>
<use>Avoid side effects; use derivations for build actions</use>
</concept>

<concept name="attribute_sets">
<description>Primary data structure in Nix</description>
<pattern name="definition">{ attr1 = value1; attr2 = value2; }</pattern>
<pattern name="access">set.attr or set."attr-with-dashes"</pattern>
<pattern name="recursive">rec { a = 1; b = a + 1; }</pattern>
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
<example>with pkgs; [ git vim tmux ]</example>
<warning>Avoid nested with; prefer explicit references for clarity</warning>
</pattern>

<pattern name="inherit">
<description>Copy attributes from another set</description>
<example>{ inherit (pkgs) git vim; inherit name version; }</example>
</pattern>

<pattern name="overlay">
<description>Modify or extend nixpkgs</description>
<example>
final: prev: {
  myPackage = prev.myPackage.override { ... };
}
</example>
</pattern>

<pattern name="callPackage">
<description>Dependency injection pattern</description>
<example>myPackage = pkgs.callPackage ./package.nix { };</example>
</pattern>
</patterns>

<derivations>
<pattern name="mkDerivation">
<description>Standard package builder</description>
<required>pname, version, src</required>
<phases>unpackPhase, patchPhase, configurePhase, buildPhase, installPhase</phases>
</pattern>

<pattern name="build_inputs">
<item name="nativeBuildInputs">Tools run at build time (compilers, build tools)</item>
<item name="buildInputs">Libraries linked at runtime</item>
</pattern>
</derivations>

<modules>
<pattern name="options_config">
<description>NixOS/Home Manager module structure</description>
<structure>
{ config, lib, pkgs, ... }:
{
  options.myModule = { ... };
  config = lib.mkIf config.myModule.enable { ... };
}
</structure>
</pattern>

<pattern name="mkOption">
<attributes>type, default, description, example</attributes>
<common_types>lib.types.bool, lib.types.str, lib.types.listOf, lib.types.attrsOf</common_types>
</pattern>

<pattern name="mkEnableOption">
<description>Shorthand for boolean enable option</description>
<example>enable = lib.mkEnableOption "my service";</example>
</pattern>
</modules>

<anti_patterns>
<avoid name="impure_paths">Use fetchurl/fetchFromGitHub instead of direct paths</avoid>
<avoid name="nested_with">Prefer explicit attribute access for clarity</avoid>
<avoid name="rec_overuse">Use let-in for complex recursive definitions</avoid>
<avoid name="string_interpolation_abuse">Use lib functions for path manipulation</avoid>
</anti_patterns>
</nix_language>

<flakes>
<structure>
{
  description = "Project description";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs: {
    # output attributes
  };
}
</structure>

<outputs>
<output name="packages">Derivations for nix build</output>
<output name="devShells">Development environments for nix develop</output>
<output name="apps">Runnable applications for nix run</output>
<output name="overlays">Nixpkgs overlays</output>
<output name="nixosModules">NixOS modules</output>
<output name="homeManagerModules">Home Manager modules</output>
<output name="nixosConfigurations">Full NixOS system configurations</output>
<output name="homeConfigurations">Home Manager configurations</output>
</outputs>

<inputs>
<pattern name="github">
<example>nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";</example>
<variants>github:owner/repo, github:owner/repo/branch, github:owner/repo/rev</variants>
</pattern>

<pattern name="follows">
<description>Share input between flakes</description>
<example>
home-manager = {
  url = "github:nix-community/home-manager";
  inputs.nixpkgs.follows = "nixpkgs";
};
</example>
</pattern>

<pattern name="flake_false">
<description>Non-flake inputs</description>
<example>
my-source = {
  url = "github:owner/repo";
  flake = false;
};
</example>
</pattern>
</inputs>

<output_patterns>
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
</pattern>

<pattern name="devShell">
<description>Development environment</description>
<example>
devShells.default = pkgs.mkShell {
  packages = with pkgs; [ nodejs yarn ];
  shellHook = ''
    echo "Development environment ready"
  '';
};
</example>
</pattern>
</output_patterns>

<commands>
<command name="nix flake update">Update all inputs</command>
<command name="nix flake update input-name">Update specific input</command>
<command name="nix flake show">Show flake outputs</command>
<command name="nix flake check">Validate flake</command>
</commands>
</flakes>

<home_manager>
<module_structure>
<standard>
{ config, pkgs, lib, ... }:
{
  options.custom.feature = {
    enable = lib.mkEnableOption "feature description";
  };

  config = lib.mkIf config.custom.feature.enable {
    # configuration when enabled
  };
}
</standard>

<file_organization>
<pattern name="by_program">home-manager/programs/git.nix</pattern>
<pattern name="by_category">home-manager/development/default.nix</pattern>
<pattern name="imports">Use imports = [ ./module1.nix ./module2.nix ];</pattern>
</file_organization>
</module_structure>

<programs>
<pattern name="basic_enable">programs.git.enable = true;</pattern>

<pattern name="with_options">
programs.git = {
  enable = true;
  userName = "name";
  userEmail = "email";
  extraConfig = { ... };
};
</pattern>

<pattern name="package_override">
programs.git = {
  enable = true;
  package = pkgs.gitFull;
};
</pattern>
</programs>

<common_modules>
<module name="programs.git">enable, userName, userEmail, signing, aliases, extraConfig</module>
<module name="programs.neovim">enable, viAlias, vimAlias, plugins, extraConfig, extraLuaConfig</module>
<module name="programs.fish">enable, shellInit, shellAliases, functions, plugins</module>
<module name="programs.tmux">enable, terminal, keyMode, plugins, extraConfig</module>
<module name="programs.direnv">enable, nix-direnv.enable, enableBashIntegration</module>
</common_modules>

<file_management>
<pattern name="home.file">
<description>Manage dotfiles directly</description>
<example>
home.file.".config/app/config" = {
  source = ./config;
  # or
  text = "content";
};
</example>
</pattern>

<pattern name="xdg.configFile">
<description>XDG config directory files</description>
<example>xdg.configFile."app/config".source = ./config;</example>
</pattern>
</file_management>

<session>
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
<description>Add to PATH</description>
<example>home.sessionPath = [ "$HOME/.local/bin" ];</example>
</pattern>
</session>

<rules>
<rule>Use programs.* when available instead of manual configuration</rule>
<rule>Group related configurations in separate modules</rule>
<rule>Use lib.mkIf for conditional configuration</rule>
<rule>Prefer xdg.configFile over home.file for XDG-compliant apps</rule>
<rule>Use home.packages for additional packages not configured via programs.*</rule>
</rules>
</home_manager>

<nixos>
<pattern name="basic">
nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ./configuration.nix
    home-manager.nixosModules.home-manager
  ];
  specialArgs = { inherit inputs; };
};
</pattern>

<pattern name="standalone_home_manager">
homeConfigurations."user@host" = home-manager.lib.homeManagerConfiguration {
  pkgs = nixpkgs.legacyPackages.x86_64-linux;
  modules = [ ./home.nix ];
  extraSpecialArgs = { inherit inputs; };
};
</pattern>

<pattern name="as_nixos_module">
home-manager.nixosModules.home-manager
{
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.username = import ./home.nix;
}
</pattern>
</nixos>

