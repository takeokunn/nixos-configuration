{ pkgs, lib, config, ... }:
let cfg = config.programs.nyxt;
in with lib; {
  options.programs.nyxt = {
    enable = mkEnableOption "";
    package = mkPackageOption pkgs "nyxt" { };
  };

  config = mkIf cfg.enable { home.packages = [ cfg.package ]; };
}
