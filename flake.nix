{
  description = "takeokunn's nix configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    org-babel.url = "github:emacs-twist/org-babel";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    lanzaboote.url = "github:nix-community/lanzaboote/v1.2.0";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:nix-community/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    # Pinned to a specific nixpkgs commit: newer nixpkgs breaks pty permissions here.
    nix-on-droid.url = "github:nix-community/nix-on-droid";
    nix-on-droid.inputs.nixpkgs.url = "github:NixOS/nixpkgs/2bceeb45e516fc6956714014c92ddfdafe4c9da3";
    nix-on-droid.inputs.home-manager.follows = "home-manager";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    mcp-servers-nix.url = "github:natsukium/mcp-servers-nix";
    mcp-servers-nix.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.url = "github:nix-community/impermanence";
    impermanence.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.inputs.home-manager.follows = "home-manager";
    nixvim.url = "github:nix-community/nixvim/main";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    # llm-agents and cclens skip nixpkgs follows: their binary caches only hold
    # builds against each flake's own pinned nixpkgs.
    llm-agents.url = "github:numtide/llm-agents.nix";
    llm-agents.inputs.treefmt-nix.follows = "treefmt-nix";
    cclens.url = "github:lambdalisue/cclens";
    agent-skills.url = "github:Kyure-A/agent-skills-nix";
    agent-skills.inputs.nixpkgs.follows = "nixpkgs";
    guard-and-guide.url = "github:kawarimidoll/guard-and-guide";
    guard-and-guide.inputs.nixpkgs.follows = "nixpkgs";
    anthropic-skills.url = "github:anthropics/skills";
    anthropic-skills.flake = false;
    paredit-cli-skills.url = "github:takeokunn/paredit-cli";
    paredit-cli-skills.flake = false;
    aitools-skills.url = "github:nerima-lisp/aitools";
    aitools-skills.flake = false;
    yomiyasu-skill.url = "github:nanaism/yomiyasu";
    yomiyasu-skill.flake = false;
    nur-packages.url = "github:takeokunn/nur-packages";
    nur-packages.inputs.nixpkgs.follows = "nixpkgs";
    darwin-vz-nix.url = "github:takeokunn/darwin-vz-nix";
    darwin-vz-nix.inputs.nixpkgs.follows = "nixpkgs";
    git-bulk-clean.url = "github:takeokunn/git-bulk-clean/v0.6.1";
    git-bulk-clean.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      flake-parts,
      treefmt-nix,
      ...
    }@inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      imports = [ treefmt-nix.flakeModule ];

      flake.darwinConfigurations.M4-Max = import ./hosts/M4-Max { inherit inputs; };
      flake.nixosConfigurations.X13Gen2 = import ./hosts/X13Gen2 { inherit inputs; };
      flake.nixOnDroidConfigurations.OPPO-A79 = import ./hosts/OPPO-A79 { inherit inputs; };

      # Category bundles auto-import nur; consumers must NOT set
      # _module.args.nurPkgs themselves.
      flake.homeManagerModules =
        let
          # Evaluates the nur-packages input against the consumer's pkgs and
          # exposes it as `nurPkgs` to every module in the tree.
          nur =
            { pkgs, ... }:
            {
              _module.args.nurPkgs = import inputs.nur-packages {
                inherit pkgs;
                aitoolsPackage =
                  inputs.nur-packages.legacyPackages.${pkgs.stdenv.hostPlatform.system}.aitools or null;
              };
            };

          # Requires the agent-skills home module
          # (inputs.agent-skills.homeManagerModules.default) alongside this.
          ai-tools =
            { pkgs, ... }:
            {
              imports = [
                nur
                ./home-manager/ai-tools
              ];
              # per-user pkgs only; useGlobalPkgs consumers must add these overlays at the system level.
              nixpkgs.overlays = [
                inputs.mcp-servers-nix.overlays.default
              ]
              ++ import ./home-manager/ai-tools/serena/overlay.nix;
              _module.args = {
                llmAgentsPkgs = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
                cclensPkg = inputs.cclens.packages.${pkgs.stdenv.hostPlatform.system}.default;
                inherit (inputs)
                  mcp-servers-nix
                  anthropic-skills
                  paredit-cli-skills
                  aitools-skills
                  yomiyasu-skill
                  ;
              };
            };
        in
        {
          inherit nur ai-tools;

          fish = ./home-manager/shell/fish; # needs nur
          tmux = ./home-manager/shell/tmux; # needs nur
          bat = ./home-manager/shell/bat; # needs nur
          bottom = ./home-manager/shell/bottom;
          direnv = ./home-manager/shell/direnv;
          eza = ./home-manager/shell/eza;
          fd = ./home-manager/shell/fd;
          fzf = ./home-manager/shell/fzf;
          jq = ./home-manager/shell/jq;
          man = ./home-manager/shell/man;
          readline = ./home-manager/shell/readline;
          ripgrep = ./home-manager/shell/ripgrep;
          zoxide = ./home-manager/shell/zoxide;
          kitty = ./home-manager/shell/kitty;
          wget = ./home-manager/shell/wget;

          git = ./home-manager/vcs/git;
          gh = ./home-manager/vcs/gh;
          tig = ./home-manager/vcs/tig; # needs nur
          git-hooks = ./home-manager/vcs/modules/git-hooks;

          gnupg = ./home-manager/security/gnupg;
          gpg-agent = ./home-manager/security/gpg-agent;
          ssh = ./home-manager/security/ssh;
          password-store = ./home-manager/security/password-store;
          gitleaks = ./home-manager/security/modules/gitleaks;

          cargo = ./home-manager/development/cargo;
          doggo = ./home-manager/development/doggo;
          lnav = ./home-manager/development/lnav;
          pandoc = ./home-manager/development/pandoc;

          # `nixvim` requires inputs.nixvim.homeModules.nixvim + nur.
          nixvim = ./home-manager/editor/nixvim;
          vim = ./home-manager/editor/vim;
          editorconfig = ./home-manager/editor/editorconfig;

          shell = {
            imports = [
              nur
              ./home-manager/shell/basic.nix
              ./home-manager/shell/advanced.nix
            ];
          };
          vcs = {
            imports = [
              nur
              ./home-manager/security/modules/gitleaks
              ./home-manager/vcs/basic.nix
              ./home-manager/vcs/advanced.nix
            ];
          };
          editor = {
            imports = [
              nur
              ./home-manager/editor/basic.nix
            ];
          };
          development = {
            imports = [
              nur
              ./home-manager/development/basic.nix
              ./home-manager/development/advanced.nix
            ];
          };
          security = {
            imports = [
              nur
              ./home-manager/security/basic.nix
              ./home-manager/security/advanced.nix
            ];
          };
        };

      perSystem =
        { pkgs, ... }:
        {
          treefmt.projectRootFile = "flake.nix";
          # sops writes these files in its own layout on every edit; yamlfmt would rewrite them back.
          treefmt.settings.global.excludes = [ "secrets/*" ];
          treefmt.programs.actionlint.enable = true;
          treefmt.programs.nixfmt.enable = true;
          treefmt.programs.taplo.enable = true;
          treefmt.programs.yamlfmt.enable = true;
          treefmt.programs.fish_indent.enable = true;
          treefmt.programs.stylua.enable = true;
          treefmt.programs.shfmt.enable = true;

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [ nixd ];
          };
        };
    };
}
