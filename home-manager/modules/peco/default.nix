{ pkgs, lib, config, ... }:
let
  cfg = config.programs.peco;
  jsonFormat = pkgs.formats.json { };
in with lib; {
  options.programs.peco = {
    enable = mkEnableOption "Simplistic interactive filtering tool";
    package = mkPackageOption pkgs "peco" { };
    config = mkOption { type = jsonFormat.type; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."peco/config.json".source =
      jsonFormat.generate "config.json" cfg.config;
  };
}
