{ pkgs, guardAndGuide }:
let
  inherit (pkgs) lib;
  shared = import ../home-manager/ai-tools/shared { inherit lib; };
  aiPromptsPath = ../home-manager/ai-tools/ai-prompts;

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
      [ "gh" "pr" "merge" ]
      [ "gh" "repo" "delete" ]
    ]
  );

  hookPath = lib.makeBinPath [ pkgs.coreutils pkgs.jq pkgs.perl pkgs.python3 ];
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
  hooksDir = pkgs.runCommand "codex-managed-hooks" { } ''
    mkdir -p "$out"
    cp ${guard} "$out/guard-and-guide"
    cp ${destructiveGit} "$out/block-destructive-git"
    cp ${rewrite} "$out/aitools-rewrite-codex"
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
        hooks = map (name: {
          type = "command";
          command = "${hooksDir}/${name}";
        }) [ "guard-and-guide" "block-destructive-git" "aitools-rewrite-codex" ];
      }
    ];
  };
}
