{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.dust;
in
with lib;
{
  options.programs.dust = {
    enable = mkEnableOption "A more intuitive version of du in rust";
    package = mkPackageOption pkgs "dust" { };
    config = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."dust/config.toml".text = cfg.config;
  };
}
