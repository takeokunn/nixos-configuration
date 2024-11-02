{ lib, config, ... }:
let
  cfg = config.programs.aerospace;
in
with lib;
{
  options.programs.aerospace = {
    enable = mkEnableOption "AeroSpace is an i3-like tiling window manager for macOS";
    config = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."aerospace/aerospace.toml".text = cfg.config;
  };
}
