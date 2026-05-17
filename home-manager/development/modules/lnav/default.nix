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
{
  options.programs.lnav = {
    enable = lib.mkEnableOption "Log file navigator";
    package = lib.mkPackageOption pkgs "lnav" { };
    config = lib.mkOption { type = jsonFormat.type; };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."lnav/config.json".source = jsonFormat.generate "config.json" cfg.config;
  };
}
