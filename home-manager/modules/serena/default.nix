{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.serena;
  yqExpr = ".ignored_paths = [${lib.concatMapStringsSep ", " (p: "\"${p}\"") cfg.ignoredPaths}]";
in
{
  options.programs.serena = {
    ignoredPaths = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      example = [
        ".devenv"
        ".direnv"
        ".terraform"
      ];
      description = ''
        List of paths to ignore across all projects in the global Serena configuration.
        Same syntax as gitignore, so you can use * and **.
        These patterns are set in ~/.serena/serena_config.yml and merged additively
        with each project's own ignored_paths.
        Requires Serena >= PR #996 (merged 2026-02-14).
      '';
    };
  };

  config = lib.mkIf (cfg.ignoredPaths != [ ]) {
    home.activation.serenaIgnoredPaths = {
      after = [ "writeBoundary" ];
      before = [ ];
      data = ''
        if [ -f "$HOME/.serena/serena_config.yml" ]; then
          ${lib.getExe pkgs.yq-go} -i '${yqExpr}' "$HOME/.serena/serena_config.yml"
        fi
      '';
    };
  };
}
