{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.gitleaks;
  tomlFormat = pkgs.formats.toml { };

  # ルール定義の型
  ruleType = lib.types.submodule {
    options = {
      id = lib.mkOption {
        type = lib.types.str;
        description = "Unique identifier for the rule";
      };
      description = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Human-readable description of the rule";
      };
      regex = lib.mkOption {
        type = lib.types.str;
        description = "Regular expression pattern to detect secrets";
      };
      path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "File path regex to restrict rule to specific files";
      };
      entropy = lib.mkOption {
        type = lib.types.nullOr lib.types.float;
        default = null;
        description = "Entropy threshold for secret detection";
      };
      secretGroup = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Regex capture group index for the actual secret";
      };
      keywords = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Keywords for pre-regex filtering";
      };
      tags = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Tags for categorization";
      };
      allowlists = lib.mkOption {
        type = lib.types.listOf allowlistType;
        default = [ ];
        description = "Rule-specific allowlists";
      };
    };
  };

  # allowlist定義の型
  allowlistType = lib.types.submodule {
    options = {
      description = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Description of the allowlist";
      };
      condition = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "AND"
            "OR"
          ]
        );
        default = null;
        description = "Condition for matching (AND requires all, OR requires any)";
      };
      regexTarget = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "match"
            "secret"
            "line"
          ]
        );
        default = null;
        description = "Target for regex matching";
      };
      regexes = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Regex patterns to match for allowlisting";
      };
      paths = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "File path patterns to allowlist";
      };
      commits = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Commit hashes to allowlist";
      };
      stopwords = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Stopwords to exclude from detection";
      };
      targetRules = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Target rules for this allowlist (global allowlists only)";
      };
    };
  };

  # ルールをTOML形式に変換
  ruleToToml =
    rule:
    let
      base = {
        inherit (rule) id description regex;
      }
      // lib.optionalAttrs (rule.path != null) { inherit (rule) path; }
      // lib.optionalAttrs (rule.entropy != null) { inherit (rule) entropy; }
      // lib.optionalAttrs (rule.secretGroup != null) { inherit (rule) secretGroup; }
      // lib.optionalAttrs (rule.keywords != [ ]) { inherit (rule) keywords; }
      // lib.optionalAttrs (rule.tags != [ ]) { inherit (rule) tags; }
      // lib.optionalAttrs (rule.allowlists != [ ]) { allowlists = map allowlistToToml rule.allowlists; };
    in
    base;

  # allowlistをTOML形式に変換
  allowlistToToml =
    al:
    lib.filterAttrs (n: v: v != null && v != [ ] && v != "") {
      inherit (al) description regexTarget condition;
      inherit (al)
        regexes
        paths
        commits
        stopwords
        targetRules
        ;
    };

  # 設定全体を構築
  configToToml =
    let
      extend = lib.optionalAttrs cfg.settings.extend.useDefault {
        extend = {
          useDefault = true;
        }
        // lib.optionalAttrs (cfg.settings.extend.disabledRules != [ ]) {
          disabledRules = cfg.settings.extend.disabledRules;
        };
      };
      rules = lib.optionalAttrs (cfg.settings.rules != [ ]) {
        rules = map ruleToToml cfg.settings.rules;
      };
      allowlists = lib.optionalAttrs (cfg.settings.allowlists != [ ]) {
        allowlists = map allowlistToToml cfg.settings.allowlists;
      };
    in
    extend // rules // allowlists;
  # pre-commitフック用のシェルスクリプト
  preCommitHook = pkgs.writeShellScript "gitleaks-pre-commit" ''
    exec ${cfg.package}/bin/gitleaks protect --staged --verbose --config ${config.xdg.configHome}/gitleaks/config.toml
  '';
in
{
  options.programs.gitleaks = {
    enable = lib.mkEnableOption "Scan git repos for secrets";

    package = lib.mkPackageOption pkgs "gitleaks" { };

    enableGitHook = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable gitleaks as git pre-commit hook";
    };

    settings = {
      extend = {
        useDefault = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Extend the default gitleaks configuration";
        };

        disabledRules = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "List of default rule IDs to disable";
          example = [
            "generic-api-key"
            "generic-password"
          ];
        };
      };

      rules = lib.mkOption {
        type = lib.types.listOf ruleType;
        default = [ ];
        description = "Custom detection rules";
        example = lib.literalExpression ''
          [
            {
              id = "custom-api-key";
              description = "Custom API Key Pattern";
              regex = "CUSTOM_[A-Z0-9]{32}";
              keywords = [ "CUSTOM_" ];
              tags = [ "custom" "api" ];
            }
          ]
        '';
      };

      allowlists = lib.mkOption {
        type = lib.types.listOf allowlistType;
        default = [ ];
        description = "Global allowlists for excluding false positives";
        example = lib.literalExpression ''
          [
            {
              description = "Ignore test files";
              paths = [ "test/.*" ".*_test\\.go$" ];
            }
          ]
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."gitleaks/config.toml".source =
      tomlFormat.generate "gitleaks-config.toml" configToToml;

    programs.git.hooks.pre-commit = lib.mkIf cfg.enableGitHook preCommitHook;
  };
}
