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
    xremap.url = "github:xremap/nix-flake";
    xremap.inputs.nixpkgs.follows = "nixpkgs";
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
    llm-agents.url = "github:numtide/llm-agents.nix";
    llm-agents.inputs.nixpkgs.follows = "nixpkgs";
    llm-agents.inputs.treefmt-nix.follows = "treefmt-nix";
    cclens.url = "github:lambdalisue/cclens";
    cclens.inputs.nixpkgs.follows = "nixpkgs";
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
      self,
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
        let
          sharedAiTools = import ./home-manager/ai-tools/shared { inherit (pkgs) lib; };

          # Guards the wiring a nix eval/build cannot: a future edit to the `permissions`
          # attrset that drops `defaultMode` or empties `deny` still evaluates and builds
          # cleanly (managed-settings.json is freeform JSON with no schema), so only an
          # assertion against the actual evaluated value catches the drift.
          claudeCodeHomeDirectory =
            self.darwinConfigurations.M4-Max.config.home-manager.users.take.home.homeDirectory;

          # The managed-settings file is system-level policy and cannot reach home-manager's
          # `configDir`, so its hook commands hardcode `<home>/.claude/hooks`. This is the only
          # scope where both values exist, so it is the only place the two can be compared.
          claudeCodeConfigDir =
            self.darwinConfigurations.M4-Max.config.home-manager.users.take.programs.claude-code.configDir;

          # Every key home-manager would place under the Claude config dir as a read-only store
          # symlink. `settings.json` must not be among them: Claude Code writes that file itself, so
          # declared settings reach it only through `mutableSettings`, which merges instead of linking.
          claudeCodeSettingsFileKeys = builtins.filter (n: pkgs.lib.hasSuffix ".claude/settings.json" n) (
            builtins.attrNames self.darwinConfigurations.M4-Max.config.home-manager.users.take.home.file
          );

          # The NixOS placement hardcodes the same shape from `/home/<user>`, and no other check
          # here evaluates that host, so without these two it ships with no drift coverage at all.
          claudeCodeLinuxHomeDirectory =
            self.nixosConfigurations.X13Gen2.config.home-manager.users.take.home.homeDirectory;

          claudeCodeLinuxConfigDir =
            self.nixosConfigurations.X13Gen2.config.home-manager.users.take.programs.claude-code.configDir;

          claudeCodeManagedSettings = import ./shared/claude-code-managed-settings.nix {
            inherit pkgs;
            inherit (pkgs) lib;
            guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
            homeDirectory = claudeCodeHomeDirectory;
          };

          claudeCodePermissions = claudeCodeManagedSettings.permissions;

          # lib.debug.runTests returns [] when every case passes, else the failing cases.
          sharedAiToolsTestFailures = pkgs.lib.debug.runTests {
            # Covers the guardrail catalog through `nix flake check` rather than relying solely on
            # the claudeBashHookNames assert in shared/claude-code-managed-settings.nix, which only
            # evaluates during a full darwinConfigurations/nixosConfigurations build.
            testGuardrailHookNames = {
              expr = sharedAiTools.guardrailHookNames;
              expected = [ "block-destructive-git" ];
            };
            testDecodeFrontmatterScalar = {
              expr = map sharedAiTools.decodeFrontmatterScalar [
                "Plain description"
                ''"Description: with colon and \"quotes\""''
              ];
              expected = [
                "Plain description"
                ''Description: with colon and "quotes"''
              ];
            };
            testAgentReadOnlyPolicies = {
              expr = map sharedAiTools.agentIsReadOnly [
                [ "tools: Read, Grep, Glob" ]
                [ "disallowedTools: Write, Edit, NotebookEdit" ]
                [
                  "tools: Read, Edit"
                  "disallowedTools: Edit"
                ]
                [ ]
                [ "tools: Read, Write" ]
                [ "tools: Read, NotebookEdit" ]
                [ "disallowedTools: Write, Edit" ]
                [ "disallowedTools: WriteFile, Edit, NotebookEdit" ]
                [ "tools: Read, WriteFile" ]
                [ "tools: Bash, mcp__serena" ]
              ];
              expected = [
                true
                true
                true
                false
                false
                false
                false
                false
                true
                true
              ];
            };
            # Codex's denyPatternToTokens cannot represent :* globs; they are listed as unenforced.
            testBashDenyPatternsUsePlainGlobs = {
              expr = builtins.filter (pkgs.lib.hasSuffix ":*") sharedAiTools.bashDenyPatterns;
              expected = [ ];
            };
          };
        in
        {
          treefmt.projectRootFile = "flake.nix";
          treefmt.programs.actionlint.enable = true;
          treefmt.programs.nixfmt.enable = true;
          treefmt.programs.taplo.enable = true;
          treefmt.programs.yamlfmt.enable = true;
          treefmt.programs.fish_indent.enable = true;
          treefmt.programs.stylua.enable = true;
          treefmt.programs.shfmt.enable = true;

          checks.shared-ai-tools-lib =
            pkgs.runCommand "shared-ai-tools-lib-tests"
              {
                failuresJson = builtins.toJSON sharedAiToolsTestFailures;
              }
              ''
                if [ "$failuresJson" != "[]" ]; then
                  echo "home-manager/ai-tools/shared regression tests failed:" >&2
                  echo "$failuresJson" >&2
                  exit 1
                fi
                touch $out
              '';

          checks.aitools-rewrite-hook =
            pkgs.runCommand "aitools-rewrite-hook-test"
              {
                nativeBuildInputs = [
                  pkgs.jq
                  pkgs.perl
                ];
              }
              ''
                bash ${./home-manager/ai-tools/ai-prompts/hooks/aitools-rewrite.test.sh} \
                  ${./home-manager/ai-tools/ai-prompts/hooks/aitools-rewrite.sh}
                touch $out
              '';

          checks.codex-ghq-trust =
            pkgs.runCommand "codex-ghq-trust-test"
              {
                nativeBuildInputs = [ pkgs.python3 ];
              }
              ''
                bash ${./home-manager/ai-tools/codex/ghq-trust.test.sh} \
                  ${./home-manager/ai-tools/codex/ghq-trust.sh}
                touch $out
              '';

          checks.gh-secret-guard =
            let
              stubGh = pkgs.writeShellScript "stub-gh" ''
                if [ "$1 $2" = "auth token" ]; then
                  printf '%s\n' "$STUB_TOKEN"
                  exit 0
                fi
                mkdir -p "$STUB_LOG"
                printf '%s\n' "$@" >"$STUB_LOG/argv"
                cat >"$STUB_LOG/stdin"
                exit "''${STUB_EXIT:-0}"
              '';
              guard = import ./home-manager/vcs/gh/gh-secret-guard.nix {
                inherit pkgs;
                gh = stubGh;
              };
              brokenGuard = import ./home-manager/vcs/gh/gh-secret-guard.nix {
                pkgs = pkgs // {
                  gitleaks = pkgs.writeShellScriptBin "gitleaks" "exit 127";
                };
                gh = stubGh;
              };
            in
            pkgs.runCommand "gh-secret-guard-test"
              {
                nativeBuildInputs = [
                  pkgs.git
                  pkgs.perl
                ];
              }
              ''
                bash ${./home-manager/vcs/gh/gh-secret-guard.test.sh} ${guard} ${brokenGuard}
                touch $out
              '';

          checks.block-destructive-git-hook =
            pkgs.runCommand "block-destructive-git-hook-test"
              {
                nativeBuildInputs = [
                  pkgs.git
                  pkgs.gitleaks
                  pkgs.jq
                  pkgs.jujutsu
                  pkgs.perl
                  pkgs.python3
                ];
              }
              ''
                bash ${./home-manager/ai-tools/ai-prompts/hooks/block-destructive-git.test.sh} \
                  ${./home-manager/ai-tools/ai-prompts/hooks/block-destructive-git.sh}
                touch $out
              '';

          checks.guard-and-guide-rules =
            pkgs.runCommand "guard-and-guide-rules-test"
              {
                nativeBuildInputs = [
                  pkgs.jq
                ];
              }
              ''
                bash ${./home-manager/ai-tools/ai-prompts/hooks/rules.test.sh} \
                  ${
                    inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default
                  }/bin/guard-and-guide \
                  ${./home-manager/ai-tools/ai-prompts/hooks/rules.toml}
                touch $out
              '';

          checks.copy-local-mcp-servers =
            pkgs.runCommand "copy-local-mcp-servers-test"
              {
                nativeBuildInputs = [
                  pkgs.jq
                  pkgs.jujutsu
                ];
              }
              ''
                bash ${./home-manager/vcs/copy-local-mcp-servers/copy-local-mcp-servers.test.sh} \
                  ${./home-manager/vcs/copy-local-mcp-servers/copy-local-mcp-servers.sh}
                touch $out
              '';

          checks.statusline =
            pkgs.runCommand "statusline-test"
              {
                nativeBuildInputs = [
                  pkgs.git
                  pkgs.jq
                  pkgs.jujutsu
                ];
              }
              ''
                bash ${./home-manager/ai-tools/ai-prompts/scripts/statusline.test.sh} \
                  ${./home-manager/ai-tools/ai-prompts/scripts/statusline.sh}
                touch $out
              '';

          checks.fish-ghq-jj =
            pkgs.runCommand "fish-ghq-jj-test"
              {
                nativeBuildInputs = [
                  pkgs.fish
                  pkgs.git
                  pkgs.jujutsu
                ];
              }
              ''
                export HOME=$(mktemp -d)
                fish ${./home-manager/shell/fish/tests/ghq-jj.test.fish} ${./home-manager/shell/fish/functions}
                touch $out
              '';

          # The whole emacs directory is copied so the test finds ../elisp/init.org.
          checks.emacs-ghq-jj =
            pkgs.runCommand "emacs-ghq-jj-test"
              {
                nativeBuildInputs = [
                  pkgs.emacs-nox
                  pkgs.git
                  pkgs.jujutsu
                ];
              }
              ''
                export HOME=$(mktemp -d)
                emacs --batch -l ${./home-manager/editor/emacs}/tests/ghq-jj-test.el \
                  -f ert-run-tests-batch-and-exit
                touch $out
              '';

          # The module is evaluated on its own with stub options, so the check needs no home
          # directory and bakes the gitleaks config path to a store path it controls.
          checks.git-hooks-pre-push =
            let
              prePush =
                (pkgs.lib.evalModules {
                  specialArgs = { inherit pkgs; };
                  modules = [
                    ./home-manager/vcs/modules/git-hooks
                    (
                      { lib, ... }:
                      {
                        options.programs.git.hooks = lib.mkOption {
                          type = lib.types.attrsOf lib.types.anything;
                          default = { };
                        };
                        options.programs.gitleaks.enable = lib.mkOption { default = true; };
                        options.programs.gitleaks.package = lib.mkOption { default = pkgs.gitleaks; };
                        options.xdg.configHome = lib.mkOption { type = lib.types.str; };
                        config.programs.gitHooks.enable = true;
                        config.xdg.configHome = "${pkgs.writeTextDir "gitleaks/config.toml" ''
                          [extend]
                          useDefault = true
                        ''}";
                      }
                    )
                  ];
                }).config.programs.git.hooks.pre-push;
            in
            pkgs.runCommand "git-hooks-pre-push-test" { nativeBuildInputs = [ pkgs.git ]; } ''
              bash ${./home-manager/vcs/modules/git-hooks/pre-push.test.sh} ${prePush}
              touch $out
            '';

          checks.claude-code-permissions =
            pkgs.runCommand "claude-code-permissions-test"
              {
                nativeBuildInputs = [ pkgs.jq ];
                permissionsJson = builtins.toJSON claudeCodePermissions;
                projectMcpEnabledJson = builtins.toJSON claudeCodeManagedSettings.enableAllProjectMcpServers;
                claudeHooksJson = builtins.unsafeDiscardStringContext (
                  builtins.toJSON claudeCodeManagedSettings.hooks
                );
                configDir = claudeCodeConfigDir;
                expectedConfigDir = "${claudeCodeHomeDirectory}/.claude";
                settingsFileKeysJson = builtins.toJSON claudeCodeSettingsFileKeys;
                homeDirectory = claudeCodeHomeDirectory;
                linuxConfigDir = claudeCodeLinuxConfigDir;
                expectedLinuxConfigDir = "${claudeCodeLinuxHomeDirectory}/.claude";
                linuxHomeDirectory = claudeCodeLinuxHomeDirectory;
              }
              ''
                defaultMode=$(jq -r '.defaultMode' <<< "$permissionsJson")
                denyCount=$(jq '.deny | length' <<< "$permissionsJson")
                settingsKeyCount=$(jq 'length' <<< "$settingsFileKeysJson")
                if [ "$defaultMode" != "bypassPermissions" ]; then
                  echo "expected managed-settings permissions.defaultMode == \"bypassPermissions\", got \"$defaultMode\"" >&2
                  exit 1
                fi
                if [ "$denyCount" -lt 1 ]; then
                  echo "managed-settings permissions.deny is unexpectedly empty" >&2
                  exit 1
                fi
                # Git writes are denied; read-only git is allowed (block-destructive-git judges it).
                for rule in 'git commit *' 'git push *' 'git reset *' 'git checkout *' 'git stash push *' 'git -C * commit *' 'git -c * push *'; do
                  if ! jq -e --arg r "Bash($rule)" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny lost \"Bash($rule)\"; agents must not run git writes" >&2
                    exit 1
                  fi
                done
                # Read-only git must stay reachable: Claude Code judges it through block-destructive-git.
                for rule in git 'git *' 'git stash *' 'git fetch' 'git fetch *' 'aitools git' 'aitools git *'; do
                  if jq -e --arg r "Bash($rule)" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny blocks read-only git again via \"Bash($rule)\"" >&2
                    exit 1
                  fi
                done
                for sub in commit push reset checkout switch rebase merge clean restore add rm mv pull clone; do
                  if ! jq -e --arg r "Bash(git $sub *)" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny lost \"Bash(git $sub *)\"" >&2
                    exit 1
                  fi
                done
                # Ask rules prompt even in bypassPermissions; push is checked by the hook.
                if ! jq -e '.ask == []' <<< "$permissionsJson" >/dev/null; then
                  echo "managed-settings permissions.ask must be empty" >&2
                  exit 1
                fi
                for rule in mcp__slite__archive-note mcp__slite__remove-blocks mcp__clickup__clickup_delete_task mcp__clickup__clickup_delete_comment mcp__clickup__clickup_delete_chat_message mcp__clickup__clickup_merge_tasks; do
                  if ! jq -e --arg r "$rule" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny lost \"$rule\"" >&2
                    exit 1
                  fi
                done
                if [ "$projectMcpEnabledJson" != false ]; then
                  echo "managed-settings must not enable all project MCP servers" >&2
                  exit 1
                fi
                # rm denials: the system directories stay denied, and scratch space stays deletable. The patterns are
                # plain globs whose only `*` is trailing, so a prefix test stands in for the matcher.
                for rule in 'Bash(rm -rf /)' 'Bash(rm -rf /etc*)' 'Bash(rm -rf /Users*)' 'Bash(rm -rf /home*)'; do
                  if ! jq -e --arg r "$rule" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny lost \"$rule\"" >&2
                    exit 1
                  fi
                done
                if ! jq -e '[.deny[] | select(startswith("Bash(rm -rf ")) | .[5:-1] | select(.[:-1] | contains("*"))] | length == 0' <<< "$permissionsJson" >/dev/null; then
                  echo "managed-settings permissions.deny holds an rm pattern with a non-trailing *, which the prefix test below cannot model" >&2
                  exit 1
                fi
                for target in 'rm -rf /tmp' 'rm -rf /tmp/x' 'rm -rf /private/tmp/x' 'rm -rf /var/folders/zz/x'; do
                  hits=$(jq -c --arg t "$target" '[.deny[] | select(startswith("Bash(rm -rf ")) | .[5:-1] | . as $p | select(if ($p | endswith("*")) then ($t | startswith($p[:-1])) else $t == $p end)]' <<< "$permissionsJson")
                  if [ "$hits" != "[]" ]; then
                    echo "managed-settings permissions.deny matches \"$target\" through $hits; scratch cleanup must stay possible" >&2
                    exit 1
                  fi
                done
                # The Claude Code hook must be told to allow a jj push; Codex's wiring passes no argument.
                if ! jq -e '[.PreToolUse[].hooks[].command | select(test("/block-destructive-git( |$)"))] | length == 1 and all(test("/block-destructive-git claude$"))' <<< "$claudeHooksJson" >/dev/null; then
                  echo "managed-settings hooks must register block-destructive-git exactly once, ending in \" claude\"" >&2
                  exit 1
                fi
                if ! jq -e '[.PreToolUse[].hooks[] | select(.command | test("/block-destructive-git( |$)|/nix/store/[^/]+-guard-and-guide$"))] | length == 2 and all(.type == "command" and .onFailure == "block")' <<< "$claudeHooksJson" >/dev/null; then
                  echo "managed-settings must register both safety hooks with onFailure=block" >&2
                  exit 1
                fi
                if [ "$configDir" != "$expectedConfigDir" ]; then
                  echo "home-manager configDir \"$configDir\" no longer matches the path the managed hook commands hardcode (\"$expectedConfigDir\")" >&2
                  exit 1
                fi
                if [ "$settingsKeyCount" -ne 0 ]; then
                  echo "home-manager is placing a read-only .claude/settings.json again; Claude Code must own that file:" >&2
                  echo "$settingsFileKeysJson" >&2
                  exit 1
                fi
                if [ "$linuxConfigDir" != "$expectedLinuxConfigDir" ]; then
                  echo "X13Gen2 configDir \"$linuxConfigDir\" no longer matches the path its managed hook commands hardcode (\"$expectedLinuxConfigDir\")" >&2
                  exit 1
                fi
                # The platform modules build the hook paths from a literal prefix, so a host whose
                # home moves off that prefix -- an impermanence-style relocation, say -- writes hook
                # commands pointing at a directory that does not exist, and nothing else notices.
                case "$homeDirectory" in
                  /Users/*) ;;
                  *) echo "M4-Max home \"$homeDirectory\" left /Users; nix-darwin/config/claude-code-settings.nix still builds its hook paths from that prefix" >&2; exit 1 ;;
                esac
                case "$linuxHomeDirectory" in
                  /home/*) ;;
                  *) echo "X13Gen2 home \"$linuxHomeDirectory\" left /home; nixos/config/claude-code-settings.nix still builds its hook paths from that prefix" >&2; exit 1 ;;
                esac
                touch $out
              '';

          imports = [
            {
              checks =
                pkgs.lib.optionalAttrs
                  (builtins.elem pkgs.stdenv.hostPlatform.system [
                    "aarch64-darwin"
                    "x86_64-linux"
                  ])
                  {
                    codex-hooks =
                      let
                        isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
                        hostConfig =
                          if isDarwin then
                            self.darwinConfigurations.M4-Max.config
                          else
                            self.nixosConfigurations.X13Gen2.config;
                        codex = hostConfig.home-manager.users.take.programs.codex;
                        linuxPolicy = hostConfig.environment.etc."codex/requirements.toml";
                        plain = builtins.unsafeDiscardStringContext;
                      in
                      pkgs.runCommand "codex-hooks-test"
                        (
                          {
                            nativeBuildInputs = [
                              pkgs.jq
                              pkgs.python3
                            ];
                            userHooksJson = plain (builtins.toJSON codex.hooks);
                            userRulesJson = plain (builtins.toJSON codex.rules);
                            settingsHooksJson = plain (builtins.toJSON (codex.settings.hooks or null));
                            settingsJson = builtins.toJSON {
                              hasAutoCompactLimit = codex.settings ? model_auto_compact_token_limit;
                              hasApprovalPolicy = codex.settings ? approval_policy;
                              hasSandboxMode = codex.settings ? sandbox_mode;
                            };
                          }
                          // pkgs.lib.optionalAttrs isDarwin {
                            darwinActivation = hostConfig.system.activationScripts.extraActivation.text;
                          }
                          // pkgs.lib.optionalAttrs (!isDarwin) {
                            requirementsFile = linuxPolicy.source;
                            requirementsMode = linuxPolicy.mode;
                          }
                        )
                        ''
                          python3 - <<'PY'
                          import json
                          import os
                          from pathlib import Path
                          import shlex
                          import tomllib

                          def check_policy(source):
                              policy = tomllib.loads(Path(source).read_text())
                              assert policy["features"]["hooks"] is True
                              hooks = policy["hooks"]
                              root = Path(hooks["managed_dir"])
                              assert root.is_absolute() and root.is_dir()
                              entries = hooks["PreToolUse"]
                              assert entries and all(entry["matcher"] == "^Bash$" for entry in entries)
                              commands = [hook["command"] for entry in entries for hook in entry["hooks"]]
                              expected = ["guard-and-guide", "block-destructive-git", "aitools-rewrite-codex"]
                              assert sorted(commands) == sorted(str(root / name) for name in expected)
                              for command in commands:
                                  path = Path(command)
                                  assert path.is_file() and not path.is_symlink() and os.access(path, os.X_OK)
                              assert "--agent claude-code" in (root / "guard-and-guide").read_text()
                              assert "aitools-rewrite.sh codex" in (root / "aitools-rewrite-codex").read_text()
                              rules = policy["rules"]["prefix_rules"]
                              assert rules and all(rule["decision"] == "forbidden" for rule in rules)
                              patterns = [[part["token"] for part in rule["pattern"]] for rule in rules]
                              for command in [["git", "commit"], ["gh", "pr", "merge"], ["gh", "repo", "delete"]]:
                                  assert command in patterns
                              assert ["git", "stash"] not in patterns
                              print(f"codex managed policy: {len(commands)} hooks, {len(rules)} forbidden prefixes")

                          # Home Manager must not register these again or recreate a rules symlink.
                          assert json.loads(os.environ["userHooksJson"]) == {}
                          assert json.loads(os.environ["settingsHooksJson"]) is None
                          assert json.loads(os.environ["userRulesJson"]) == {}
                          if "darwinActivation" in os.environ:
                              installs = []
                              for line in os.environ["darwinActivation"].splitlines():
                                  if not line.lstrip().startswith("install "):
                                      continue
                                  argv = shlex.split(line, comments=True)
                                  if argv[-1] == "/etc/codex/requirements.toml":
                                      assert len(argv) == 5 and argv[:3] == ["install", "-m", "0644"]
                                      installs.append(argv[3])
                              assert len(installs) == 1
                              check_policy(installs[0])
                          else:
                              assert os.environ["requirementsMode"] == "0644"
                              check_policy(os.environ["requirementsFile"])
                          PY
                          # approval_policy and sandbox_mode are wrapper -c flags so they outrank the mutable config.toml.
                          if ! jq -e '.hasAutoCompactLimit == false and .hasApprovalPolicy == false and .hasSandboxMode == false' <<< "$settingsJson" >/dev/null; then
                            echo "programs.codex.settings carries model_auto_compact_token_limit, approval_policy, or sandbox_mode" >&2
                            exit 1
                          fi
                          touch $out
                        '';
                  };
            }
          ];

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [ nixd ];
          };
        };
    };
}
