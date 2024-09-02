{ pkgs, lib, config, ... }:
let cfg = config.programs.wget;
in with lib; {
  options.programs.wget = {
    enable = mkEnableOption "Simplistic interactive filtering tool";
    package = mkPackageOption pkgs "wget" { };
    config = mkOption { type = types.lines; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."wgetrc".text = cfg.config;
  };
}
