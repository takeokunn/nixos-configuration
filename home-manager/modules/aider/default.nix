{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.aider;
in
with lib;
{
  options.programs.aider = {
    enable = mkEnableOption "aider is AI pair programming in your terminal";
    package = mkPackageOption pkgs "aider-chat" { };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
