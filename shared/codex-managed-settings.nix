{
  pkgs,
  guardAndGuide,
  # Absolute home directory of the governed user, for the hook home-manager installs there.
  homeDirectory,
}:
let
  inherit (pkgs) lib;
  shared = import ../home-manager/ai-tools/shared { inherit lib; };
  aiPromptsPath = ../home-manager/ai-tools/ai-prompts;
  # Installed by home-manager/ai-tools/gbrain, which asserts this path.
  gbrainHook = "${homeDirectory}/.local/share/gbrain/hook";

  # Prefix rules cannot express shell globs or distinguish `git stash` from `git stash list`.
  # Those cases remain guarded by the Bash hooks.
  denyPatternToTokens =
    pattern:
    let
      literal = lib.removeSuffix " *" pattern;
    in
    if lib.hasInfix "*" literal then null else lib.splitString " " literal;
  readOnlyGitTokens = map (r: [ "git" ] ++ lib.splitString " " r) shared.gitReadSubcommands;
  shadowsRead =
    tokens: lib.any (read: lib.take (builtins.length tokens) read == tokens) readOnlyGitTokens;
  denyTokens = lib.unique (
    lib.filter (tokens: tokens != null && !shadowsRead tokens) (
      map denyPatternToTokens shared.bashDenyPatterns
    )
    ++ [
      [
        "gh"
        "pr"
        "merge"
      ]
      [
        "gh"
        "repo"
        "delete"
      ]
      # Prefix forms of the `gh auth status *-t*` patterns, which denyPatternToTokens drops. A flag
      # placed after another argument (`gh auth status -h x -t`) still passes this layer.
      [
        "gh"
        "auth"
        "status"
        "-t"
      ]
      [
        "gh"
        "auth"
        "status"
        "--show-token"
      ]
    ]
  );

  hookPath = lib.makeBinPath [
    pkgs.coreutils
    pkgs.jq
    pkgs.perl
    pkgs.python3
  ];
  guard = pkgs.writeShellScript "guard-and-guide-codex" ''
    if ! ${guardAndGuide}/bin/guard-and-guide --agent claude-code --config ${aiPromptsPath}/hooks/rules.toml; then
      echo 'guard-and-guide failed; command blocked.' >&2
      exit 2
    fi
  '';
  destructiveGit = pkgs.writeShellScript "block-destructive-git" ''
    export PATH=${hookPath}:$PATH
    export GITLEAKS_BIN=${pkgs.gitleaks}/bin/gitleaks
    if ! ${pkgs.bash}/bin/bash ${aiPromptsPath}/hooks/block-destructive-git.sh; then
      echo 'Git guard failed or denied the command; command blocked.' >&2
      exit 2
    fi
  '';
  rewrite = pkgs.writeShellScript "aitools-rewrite-codex" ''
    export PATH=${hookPath}:$PATH
    exec ${pkgs.bash}/bin/bash ${aiPromptsPath}/hooks/aitools-rewrite.sh codex
  '';
  redact = pkgs.writeShellScript "redact-secrets-codex" ''
    export PATH=${pkgs.jq}/bin:$PATH
    exec ${pkgs.bash}/bin/bash ${aiPromptsPath}/hooks/redact-secrets.sh codex
  '';
  requirePrSkill = pkgs.writeShellScript "require-pr-skill" ''
    export PATH=${
      lib.makeBinPath [
        pkgs.coreutils
        pkgs.gnugrep
        pkgs.jq
      ]
    }:$PATH
    exec ${pkgs.bash}/bin/bash ${aiPromptsPath}/hooks/require-pr-skill.sh
  '';
  hooksDir = pkgs.runCommand "codex-managed-hooks" { } ''
    mkdir -p "$out"
    cp ${guard} "$out/guard-and-guide"
    cp ${destructiveGit} "$out/block-destructive-git"
    cp ${rewrite} "$out/aitools-rewrite-codex"
    cp ${redact} "$out/redact-secrets-codex"
    cp ${requirePrSkill} "$out/require-pr-skill"
  '';
in
{
  features.hooks = true;
  rules.prefix_rules = map (tokens: {
    pattern = map (token: { inherit token; }) tokens;
    decision = "forbidden";
  }) denyTokens;
  hooks = {
    managed_dir = "${hooksDir}";
    PreToolUse = [
      {
        matcher = "^Bash$";
        hooks =
          map
            (name: {
              type = "command";
              command = "${hooksDir}/${name}";
            })
            [
              "guard-and-guide"
              "block-destructive-git"
              "aitools-rewrite-codex"
              "require-pr-skill"
            ];
      }
    ];
    # Last layer, fail-open like every Codex hook: a failure leaves the output as it was.
    PostToolUse = [
      {
        matcher = "^Bash$";
        hooks = [
          {
            type = "command";
            command = "${hooksDir}/redact-secrets-codex";
          }
        ];
      }
    ];
    SessionStart = [
      {
        hooks = [
          {
            type = "command";
            command = "${gbrainHook} session-start --harness codex";
            timeout = 15;
          }
        ];
      }
    ];
    UserPromptSubmit = [
      {
        hooks = [
          {
            type = "command";
            command = "${gbrainHook} user-prompt --harness codex";
            timeout = 15;
          }
        ];
      }
    ];
  };
}
