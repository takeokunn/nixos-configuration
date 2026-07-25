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

    # editorconfig-checker's indentation/indent-size checks assume a fixed
    # per-level indent multiple, which Lisp's vertical-alignment convention
    # (continuation lines aligned to an enclosing form's column, not a fixed
    # tab stop) routinely and correctly violates. Run those two checks only
    # over non-Lisp staged files; everything else (charset, trailing
    # whitespace, final newline, max line length) still applies to Lisp too.
    RESULT=0
    LISP_FILES=()
    OTHER_FILES=()
    while IFS= read -r -d "" FILE; do
      case "$FILE" in
        *.lisp | *.lispworks | *.asd | *.el)
          LISP_FILES+=("$FILE")
          ;;
        *)
          OTHER_FILES+=("$FILE")
          ;;
      esac
    done < <(${pkgs.git}/bin/git diff --cached --name-only -z --diff-filter=ACMR)

    if [ ''${#OTHER_FILES[@]} -gt 0 ]; then
      ${cfg.editorconfigCheckerPackage}/bin/editorconfig-checker "''${OTHER_FILES[@]}" || RESULT=1
    fi

    if [ ''${#LISP_FILES[@]} -gt 0 ]; then
      ${cfg.editorconfigCheckerPackage}/bin/editorconfig-checker -disable-indentation -disable-indent-size "''${LISP_FILES[@]}" || RESULT=1
    fi

    exit $RESULT
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
