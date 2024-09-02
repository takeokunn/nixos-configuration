{ pkgs, lib, config, ... }:
let cfg = config.programs.peco;
in with lib; {
  options.programs.peco = {
    enable = mkEnableOption "Simplistic interactive filtering tool";
    package = mkPackageOption pkgs "peco" { };
    config = mkOption { type = types.lines; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."peco/config.json".text = cfg.config;
  };
}
