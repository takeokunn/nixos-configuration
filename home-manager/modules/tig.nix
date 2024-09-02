{ pkgs, lib, config, ... }:
let cfg = config.programs.tig;
in with lib; {
  options.programs.tig = {
    enable = mkEnableOption "Text-mode interface for git";
    package = mkPackageOption pkgs "tig" { };
    config = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".tigrc".text = cfg.config;
  };
}
