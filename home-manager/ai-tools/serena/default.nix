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

  config = lib.mkIf (cfg.ignoredPaths != [ ]) {
    home.activation.serenaIgnoredPaths.after = [ "writeBoundary" ];
    home.activation.serenaIgnoredPaths.before = [ ];
    home.activation.serenaIgnoredPaths.data = ''
      if [ -f "$HOME/.serena/serena_config.yml" ]; then
        ${lib.getExe pkgs.yq-go} -i '${yqExpr}' "$HOME/.serena/serena_config.yml"
      fi
    '';
  };
}
