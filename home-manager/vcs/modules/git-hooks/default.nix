{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.gitHooks;
  gitleaksCfg = config.programs.gitleaks;

  checkMergeConflicts = pkgs.writeShellScript "check-merge-conflicts" ''
    RESULT=0
    while IFS= read -r -d "" FILE; do
      if ${pkgs.gnugrep}/bin/grep -qnE '(<{7}|={7}|>{7})' "$FILE" 2>/dev/null; then
        printf 'Merge conflict markers found in: %s\n' "$FILE"
        RESULT=1
      fi
    done < <(${pkgs.git}/bin/git diff --cached --name-only -z --diff-filter=ACMR)
    exit $RESULT
  '';

  checkCaseConflicts = pkgs.writeShellScript "check-case-conflicts" ''
    ALL_FILES=$(${pkgs.git}/bin/git ls-files)
    RESULT=0
    while IFS= read -r -d "" staged_file; do
      staged_lower=$(printf '%s' "$staged_file" | tr '[:upper:]' '[:lower:]')
      while IFS= read -r existing_file; do
        if [ "$staged_file" != "$existing_file" ]; then
          existing_lower=$(printf '%s' "$existing_file" | tr '[:upper:]' '[:lower:]')
          if [ "$staged_lower" = "$existing_lower" ]; then
            printf 'Case conflict: %s vs %s\n' "$staged_file" "$existing_file"
            RESULT=1
          fi
        fi
      done <<< "$ALL_FILES"
    done < <(${pkgs.git}/bin/git diff --cached --name-only -z --diff-filter=ACR)
    exit $RESULT
  '';

  editorconfigCheck = pkgs.writeShellScript "editorconfig-checker-hook" ''
    if [ ! -f .editorconfig ]; then
      exit 0
    fi
    ${pkgs.git}/bin/git diff --cached --name-only -z --diff-filter=ACMR | ${pkgs.findutils}/bin/xargs -0 --no-run-if-empty ${cfg.editorconfigCheckerPackage}/bin/editorconfig-checker
  '';

  gitleaksCheck = pkgs.writeShellScript "gitleaks-hook" ''
    exec ${gitleaksCfg.package}/bin/gitleaks protect --staged --verbose --config ${config.xdg.configHome}/gitleaks/config.toml
  '';

  preCommitScript = pkgs.writeShellScript "pre-commit" ''
    RESULT=0

    ${lib.optionalString cfg.enableEditorconfigChecker ''
      ${editorconfigCheck} || RESULT=1
    ''}

    ${lib.optionalString cfg.enableCheckMergeConflicts ''
      ${checkMergeConflicts} || RESULT=1
    ''}

    ${lib.optionalString cfg.enableCheckCaseConflicts ''
      ${checkCaseConflicts} || RESULT=1
    ''}

    ${lib.optionalString (cfg.enableGitleaks && gitleaksCfg.enable) ''
      ${gitleaksCheck} || RESULT=1
    ''}

    exit $RESULT
  '';
in
{
  options.programs.gitHooks = {
    enable = lib.mkEnableOption "Unified git pre-commit hooks";

    enableEditorconfigChecker = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable editorconfig-checker in pre-commit hook";
    };

    enableCheckMergeConflicts = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable merge conflict marker detection in pre-commit hook";
    };

    enableCheckCaseConflicts = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable case-insensitive filename conflict detection in pre-commit hook";
    };

    enableGitleaks = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable gitleaks secret scanning in pre-commit hook";
    };

    editorconfigCheckerPackage = lib.mkPackageOption pkgs "editorconfig-checker" { };
  };

  config = lib.mkIf cfg.enable {
    programs.git.hooks.pre-commit = preCommitScript;
  };
}
