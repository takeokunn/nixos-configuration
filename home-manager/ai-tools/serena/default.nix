{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.serena;
  yqExpr = ''
    ${lib.optionalString (cfg.ignoredPaths != [ ]) ".ignored_paths = env(SERENA_IGNORED_PATHS) | "}
    .ls_specific_settings.typescript.ls_path = strenv(SERENA_TYPESCRIPT_LS_PATH) |
    .ls_specific_settings.bash.ls_path = strenv(SERENA_BASH_LS_PATH)
  '';
in
{
  options.programs.serena = {
    ignoredPaths = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "**/.devenv/**"
        "**/.direnv/**"
        "**/.terraform/**"
      ];
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
        Defaults to the common build/tooling directories every consumer previously
        declared individually. Any explicit assignment of this option already
        replaces the default outright (listOf's default applies only when no
        module sets the option explicitly); lib.mkForce is only needed to resolve
        a conflict between two modules that both assign it explicitly. A consumer
        that wants to add to the baseline rather than replace it must repeat these
        three entries itself, since the default is not merged in alongside an
        explicit definition.
        Requires Serena >= PR #996 (merged 2026-02-14).
      '';
    };
  };

  config = {
    home.activation.serenaSettings.after = [ "writeBoundary" ];
    home.activation.serenaSettings.before = [ ];
    home.activation.serenaSettings.data = ''
      serenaConfigFile="$HOME/.serena/serena_config.yml"
      if [ -f "$serenaConfigFile" ]; then
        SERENA_IGNORED_PATHS=${lib.escapeShellArg (builtins.toJSON cfg.ignoredPaths)} \
          SERENA_TYPESCRIPT_LS_PATH=${lib.escapeShellArg "${pkgs.typescript-language-server}/bin/typescript-language-server"} \
          SERENA_BASH_LS_PATH=${lib.escapeShellArg "${pkgs.bash-language-server}/bin/bash-language-server"} \
          ${lib.getExe pkgs.yq-go} -i '${yqExpr}' "$serenaConfigFile"

        ${lib.getExe pkgs.yq-go} -r '.projects[]?' "$serenaConfigFile" |
          while IFS= read -r project; do
            if [ ! -d "$project" ]; then
              SERENA_PROJECT_PATH="$project" ${lib.getExe pkgs.yq-go} -i \
                '.projects = (.projects // [] | map(select(. != strenv(SERENA_PROJECT_PATH))))' \
                "$serenaConfigFile"
            fi
          done
      fi
    '';
  };
}
