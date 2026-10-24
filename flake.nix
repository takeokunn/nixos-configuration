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
                guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
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
          dust = ./home-manager/shell/dust;
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
          gh-dash = ./home-manager/vcs/gh-dash;
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

          # Every key home-manager would still place under the Claude config dir as a read-only
          # store symlink. `settings.json` must not be among them: Claude Code writes that file
          # itself, and a store symlink makes the write fail silently.
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
            testAgentOpencodePermissions = {
              expr = sharedAiTools.agentOpencodePermissions [ "tools: Read, Grep, Glob" ];
              expected = ''
                permission:
                  '*': deny
                  read: allow
                  glob: allow
                  grep: allow
              '';
            };
            testWritableAgentPermissions = {
              expr = sharedAiTools.agentOpencodePermissions [ "tools: Read, Write" ];
              expected = "";
            };
            # opencode's schema names URL servers "remote"; "http" is Claude Code's spelling.
            testMcpServerToOpencodeRemote = {
              expr = sharedAiTools.mcpServerToOpencode {
                type = "http";
                url = "https://example.com/mcp";
              };
              expected = {
                type = "remote";
                url = "https://example.com/mcp";
              };
            };
            testMcpServerToOpencodeLocalNoArgs = {
              expr = sharedAiTools.mcpServerToOpencode { command = "/bin/foo"; };
              expected = {
                type = "local";
                command = [ "/bin/foo" ];
              };
            };
            testMcpServerToOpencodeLocalWithArgs = {
              expr = sharedAiTools.mcpServerToOpencode {
                command = "/bin/foo";
                args = [
                  "a"
                  "b"
                ];
              };
              expected = {
                type = "local";
                command = [
                  "/bin/foo"
                  "a"
                  "b"
                ];
              };
            };
            # opencode has no `:*` form, so such an entry would reach it as a literal that never matches.
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
                  pkgs.jq
                  pkgs.perl
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
                nativeBuildInputs = [ pkgs.jq ];
              }
              ''
                bash ${./home-manager/ai-tools/ai-prompts/hooks/rules.test.sh} \
                  ${
                    inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default
                  }/bin/guard-and-guide \
                  ${./home-manager/ai-tools/ai-prompts/hooks/rules.toml}
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
                autoModeJson = builtins.toJSON claudeCodeManagedSettings.autoMode;
                # The module option, not the built opencode.json: a Linux runner can evaluate the Mac
                # host but cannot build the file, whose MCP server packages are Mac builds.
                opencodeBashPermissionsJson = builtins.toJSON self.darwinConfigurations.M4-Max.config.home-manager.users.take.programs.opencode.settings.permission.bash;
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
                bypass=$(jq -r '.disableBypassPermissionsMode' <<< "$permissionsJson")
                settingsKeyCount=$(jq 'length' <<< "$settingsFileKeysJson")
                if [ "$defaultMode" != "auto" ]; then
                  echo "expected managed-settings permissions.defaultMode == \"auto\", got \"$defaultMode\"" >&2
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
                  # opencode runs none of the hooks, so its prefix deny is the only thing between it and a git write.
                  if ! jq -e --arg r "$rule" '.[$r] == "deny"' <<< "$opencodeBashPermissionsJson" >/dev/null; then
                    echo "opencode permission.bash no longer denies \"$rule\"" >&2
                    exit 1
                  fi
                done
                # Read-only git must stay reachable: Claude Code judges it through block-destructive-git, and
                # opencode asks on an unknown git command but allows the listed reads.
                for rule in git 'git *' 'git stash *' 'aitools git' 'aitools git *'; do
                  if jq -e --arg r "Bash($rule)" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny blocks read-only git again via \"Bash($rule)\"" >&2
                    exit 1
                  fi
                done
                if ! jq -e '.["git *"] == "ask" and .["git"] == "ask"' <<< "$opencodeBashPermissionsJson" >/dev/null; then
                  echo "opencode permission.bash lost its \"git\"/\"git *\" ask default, which covers git writes no prefix can name" >&2
                  exit 1
                fi
                for rule in 'git status *' 'git status' 'git stash list'; do
                  if ! jq -e --arg r "$rule" '.[$r] == "allow"' <<< "$opencodeBashPermissionsJson" >/dev/null; then
                    echo "opencode permission.bash no longer allows \"$rule\"" >&2
                    exit 1
                  fi
                done
                for sub in commit push reset checkout switch rebase merge clean restore add rm mv pull fetch clone; do
                  if ! jq -e --arg r "Bash(git $sub *)" '.deny | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.deny lost \"Bash(git $sub *)\"" >&2
                    exit 1
                  fi
                  for rule in "git $sub" "git $sub *"; do
                    if ! jq -e --arg r "$rule" '.[$r] == "deny"' <<< "$opencodeBashPermissionsJson" >/dev/null; then
                      echo "opencode permission.bash no longer denies \"$rule\"" >&2
                      exit 1
                    fi
                  done
                done
                # Each of these runs a program or leaks the environment from inside an allowed command.
                for rule in 'Bash(nix eval *)' 'Bash(nix flake show *)' 'Bash(nix flake metadata *)' 'Bash(jq *)' 'Bash(tree *)' 'Bash(aitools sys procs *)' 'Bash(bash *)' 'Bash(sh *)' 'Bash(python *)' 'Bash(python3 *)' 'Bash(node *)'; do
                  if jq -e --arg r "$rule" '.allow | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.allow contains \"$rule\", which skips the classifier for a command that can run code or leak secrets" >&2
                    exit 1
                  fi
                done
                if ! jq -e '.ask | index("Bash(rg *--hostname-bin*)")' <<< "$permissionsJson" >/dev/null; then
                  echo "managed-settings permissions.ask lost \"Bash(rg *--hostname-bin*)\"" >&2
                  exit 1
                fi
                # Auto mode ignores a blanket Bash allow, and the push and PR checkpoints must stay ask rules.
                for rule in 'Bash(*)' 'Bash'; do
                  if jq -e --arg r "$rule" '.allow | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.allow contains the blanket \"$rule\"" >&2
                    exit 1
                  fi
                done
                for rule in 'Bash(jj git push*)' 'Bash(gh pr create*)' 'Bash(gh pr merge*)'; do
                  if ! jq -e --arg r "$rule" '.ask | index($r)' <<< "$permissionsJson" >/dev/null; then
                    echo "managed-settings permissions.ask lost \"$rule\"" >&2
                    exit 1
                  fi
                done
                if ! jq -e '.environment | index("$defaults")' <<< "$autoModeJson" >/dev/null; then
                  echo "managed-settings autoMode.environment lost \"\$defaults\", which drops the built-in trust slots" >&2
                  exit 1
                fi
                # "$defaults" plus the four custom entries (source control, worktrees, scratch, contest servers).
                if [ "$(jq '.environment | length' <<< "$autoModeJson")" -ne 5 ]; then
                  echo "managed-settings autoMode.environment no longer has \"\$defaults\" plus 4 custom entries" >&2
                  exit 1
                fi
                if jq -e '.environment[] | select(contains("/Users/") or contains("/home/"))' <<< "$autoModeJson" >/dev/null; then
                  echo "managed-settings autoMode.environment names an absolute home path" >&2
                  exit 1
                fi
                if [ "$(jq '.allow | length' <<< "$permissionsJson")" -lt 1 ]; then
                  echo "managed-settings permissions.allow is unexpectedly empty" >&2
                  exit 1
                fi
                # Deny wins over allow, so a rule in both lists is an allow entry that never applies.
                overlap=$(jq -c '[.allow[] as $a | .deny[] | select(. == $a)]' <<< "$permissionsJson")
                if [ "$overlap" != "[]" ]; then
                  echo "managed-settings rules in both allow and deny: $overlap" >&2
                  exit 1
                fi
                if [ "$bypass" != "disable" ]; then
                  echo "expected managed-settings permissions.disableBypassPermissionsMode == \"disable\", got \"$bypass\"" >&2
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

          checks.codex-hooks =
            let
              codex = self.darwinConfigurations.M4-Max.config.home-manager.users.take.programs.codex;
              # The hook scripts and the MCP server packages are Mac builds, which a Linux runner could
              # not realise; only their text is inspected here.
              plain = builtins.unsafeDiscardStringContext;
            in
            pkgs.runCommand "codex-hooks-test"
              {
                nativeBuildInputs = [ pkgs.jq ];
                hooksJson = plain (builtins.toJSON codex.hooks);
                settingsHooksJson = plain (builtins.toJSON (codex.settings.hooks or null));
                settingsJson = builtins.toJSON {
                  inherit (codex.settings) model model_reasoning_effort;
                  hasAutoCompactLimit = codex.settings ? model_auto_compact_token_limit;
                  hasApprovalPolicy = codex.settings ? approval_policy;
                  hasSandboxMode = codex.settings ? sandbox_mode;
                };
                rulesDeny = plain codex.rules.deny;
              }
              ''
                # Codex merges hook sources, so a hook registered through both programs.codex.hooks and
                # settings.hooks would run twice.
                if [ "$settingsHooksJson" != "null" ]; then
                  echo "programs.codex.settings.hooks is set next to programs.codex.hooks" >&2
                  exit 1
                fi
                n=$(jq '[.. | objects | select(.command? != null) | .command | select(test("-block-destructive-git$"))] | length' <<< "$hooksJson")
                if [ "$n" -ne 1 ]; then
                  echo "programs.codex.hooks registers block-destructive-git $n times, expected exactly once" >&2
                  exit 1
                fi
                n=$(jq '[.. | objects | select(.command? != null) | .command | select(test("-aitools-rewrite-codex$"))] | length' <<< "$hooksJson")
                if [ "$n" -ne 1 ]; then
                  echo "programs.codex.hooks registers aitools-rewrite-codex $n times, expected exactly once" >&2
                  exit 1
                fi
                # The bare aitools-rewrite script emits Claude Code's output shape, which Codex rejects.
                n=$(jq '[.. | objects | select(.command? != null) | .command | select(test("aitools-rewrite"))] | length' <<< "$hooksJson")
                if [ "$n" -ne 1 ]; then
                  echo "programs.codex.hooks references aitools-rewrite $n times, expected only the codex wrapper" >&2
                  exit 1
                fi
                if ! jq -e '.PreToolUse | length > 0 and all(.[]; .matcher == "^Bash$")' <<< "$hooksJson" >/dev/null; then
                  echo "programs.codex.hooks.PreToolUse must be non-empty with every matcher exactly ^Bash$" >&2
                  exit 1
                fi
                if jq -e '.. | objects | select(.command? != null) | .command | select(test("block-bare-cd|prose-skill-gate"))' <<< "$hooksJson" >/dev/null; then
                  echo "programs.codex.hooks wires a removed hook (block-bare-cd or prose-skill-gate)" >&2
                  exit 1
                fi
                if ! jq -e '.model == "gpt-6.1-sol" and .model_reasoning_effort == "medium"' <<< "$settingsJson" >/dev/null; then
                  echo "programs.codex.settings model/model_reasoning_effort drifted from gpt-6.1-sol/medium" >&2
                  exit 1
                fi
                # approval_policy and sandbox_mode are wrapper -c flags so they outrank the mutable config.toml.
                if ! jq -e '.hasAutoCompactLimit == false and .hasApprovalPolicy == false and .hasSandboxMode == false' <<< "$settingsJson" >/dev/null; then
                  echo "programs.codex.settings carries model_auto_compact_token_limit, approval_policy, or sandbox_mode" >&2
                  exit 1
                fi
                # The bare pattern "git stash" would become this prefix rule and forbid `git stash list`.
                if grep -F 'prefix_rule(pattern = ["git","stash"],' <<< "$rulesDeny" >/dev/null; then
                  echo "programs.codex.rules.deny forbids the bare [\"git\",\"stash\"] prefix, which also covers git stash list" >&2
                  exit 1
                fi
                if ! grep -F 'prefix_rule(pattern = ["git","commit"],' <<< "$rulesDeny" >/dev/null; then
                  echo "programs.codex.rules.deny no longer forbids git commit" >&2
                  exit 1
                fi
                touch $out
              '';

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [ nixd ];
          };
        };
    };
}
