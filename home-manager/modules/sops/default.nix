{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.sops;
in
with lib;
{
  options.programs.sops = {
    enable = mkEnableOption "Simple and flexible tool for managing secrets";
    package = mkPackageOption pkgs "sops" { };
  };

  config = mkIf cfg.enable { home.packages = [ cfg.package ]; };
}
