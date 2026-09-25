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
      if ${pkgs.gnugrep}/bin/grep -qnE '^(<{7}([[:space:]]|$)|={7}$|>{7}([[:space:]]|$))' "$FILE" 2>/dev/null; then
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

  # Agents have committed as "take <...noreply...>" or with a misspelled email by
  # passing `git -c user.*` or GIT_AUTHOR_*/GIT_COMMITTER_* overrides. The expected
  # identity is what the config files resolve to (so includeIf work identities
  # still apply), read with command-line config overrides stripped. Scoped to
  # the ghq root so test fixtures committing as a fake identity in temp repos
  # keep working.
  identityPrelude = ''
    git_from_files() {
      env -u GIT_CONFIG_PARAMETERS -u GIT_CONFIG_COUNT ${pkgs.git}/bin/git "$@"
    }

    GHQ_ROOT=$(git_from_files config --path ghq.root) || exit 0
    GHQ_ROOT=$(cd "$GHQ_ROOT" 2>/dev/null && pwd -P) || exit 0
    COMMON_DIR=$(cd "$(${pkgs.git}/bin/git rev-parse --git-common-dir)" && pwd -P) || exit 0
    case "$COMMON_DIR/" in
      "$GHQ_ROOT"/*) ;;
      *) exit 0 ;;
    esac

    EXPECTED_NAME=$(git_from_files config user.name)
    EXPECTED_EMAIL=$(git_from_files config user.email)
    if [ -z "$EXPECTED_NAME" ] || [ -z "$EXPECTED_EMAIL" ]; then
      printf 'user.name and user.email must be set in git config files\n'
      exit 1
    fi
    EXPECTED="$EXPECTED_NAME <$EXPECTED_EMAIL>"
  '';

  checkCommitIdentity = pkgs.writeShellScript "check-commit-identity" ''
    ${identityPrelude}

    RESULT=0
    for VAR in GIT_AUTHOR_IDENT GIT_COMMITTER_IDENT; do
      IDENT=$(${pkgs.git}/bin/git var "$VAR") || exit 1
      ACTUAL=''${IDENT% * *}
      if [ "$ACTUAL" != "$EXPECTED" ]; then
        printf '%s is "%s", expected "%s" from git config files\n' "$VAR" "$ACTUAL" "$EXPECTED"
        RESULT=1
      fi
    done
    exit $RESULT
  '';

  prePushScript = pkgs.writeShellScript "pre-push" ''
    ${identityPrelude}

    # Only commits the remote does not already have are checked, so history
    # that landed before this hook existed does not block every push. The
    # remote's refs are listed directly because remote-tracking refs are absent
    # in clones without a fetch refspec.
    REMOTE_OIDS=$(${pkgs.git}/bin/git ls-remote "$1") || exit 1
    KNOWN=$(
      while read -r OID _; do
        printf '%s\n' "$OID"
      done <<< "$REMOTE_OIDS" | ${pkgs.git}/bin/git cat-file --batch-check='^%(objectname)' | ${pkgs.gnugrep}/bin/grep -v ' missing$'
    )

    RESULT=0
    while read -r _ LOCAL_OID _ _; do
      if [ "$LOCAL_OID" = "$(printf '%0*d' "''${#LOCAL_OID}" 0)" ]; then
        continue
      fi
      while IFS=$'\t' read -r OID AUTHOR COMMITTER; do
        if [ "$AUTHOR" != "$EXPECTED" ] || [ "$COMMITTER" != "$EXPECTED" ]; then
          printf '%s: author "%s", committer "%s", expected "%s"\n' "$OID" "$AUTHOR" "$COMMITTER" "$EXPECTED"
          RESULT=1
        fi
      done < <(printf '%s\n' "$KNOWN" | ${pkgs.git}/bin/git log --stdin --format='%h%x09%an <%ae>%x09%cn <%ce>' "$LOCAL_OID")
    done
    exit $RESULT
  '';

  preCommitScript = pkgs.writeShellScript "pre-commit" ''
    RESULT=0

    ${lib.optionalString cfg.enableIdentityCheck ''
      ${checkCommitIdentity} || RESULT=1
    ''}

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
      # Off by default: the check is O(tracked files x staged files).
      default = false;
      description = "Enable case-insensitive filename conflict detection in pre-commit hook";
    };

    enableGitleaks = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable gitleaks secret scanning in pre-commit hook";
    };

    enableIdentityCheck = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Reject commits and pushes whose author or committer differs from the identity in git config files";
    };

    editorconfigCheckerPackage = lib.mkPackageOption pkgs "editorconfig-checker" { };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      { programs.git.hooks.pre-commit = preCommitScript; }
      (lib.mkIf cfg.enableIdentityCheck {
        programs.git.hooks.pre-merge-commit = checkCommitIdentity;
        programs.git.hooks.pre-push = prePushScript;
      })
    ]
  );
}
