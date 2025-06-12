{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.aider;
  yamlFormat = pkgs.formats.yaml { };
in
with lib;
{
  options.programs.aider = {
    enable = mkEnableOption "aider is AI pair programming in your terminal";
    package = mkPackageOption pkgs "aider-chat" { };
    config = mkOption { type = yamlFormat.type; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".aider.conf.yml".source = yamlFormat.generate ".aider.conf.yml" cfg.config;
  };
}
