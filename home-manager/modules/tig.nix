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
    plugins = mkOption {
      type = types.listOf pluginModule;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".tigrc".text = cfg.config;
    xdg.configFile = mkMerge
      ((map (plugin: { ".tig/${plugin.name}".source = plugin.src; })
        cfg.plugins));
  };
}
