{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.lnav;
  jsonFormat = pkgs.formats.json { };
in
with lib;
{
  options.programs.lnav = {
    enable = mkEnableOption "Log file navigator";
    package = mkPackageOption pkgs "lnav" { };
    config = mkOption { type = jsonFormat.type; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."lnav/config.json".source = jsonFormat.generate "config.json" cfg.config;
  };
}
