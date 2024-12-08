{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.aerospace;
  settingsFormat = pkgs.formats.toml { };
in
with lib;
{
  options.programs.aerospace = {
    enable = mkEnableOption "AeroSpace is an i3-like tiling window manager for macOS";

    settings = mkOption {
      type = settingsFormat.type;
      example = lib.literalExpression ''
        {
          gaps = {
            outer.left = 8;
            outer.bottom = 8;
            outer.top = 8;
            outer.right = 8;
          };
          mode.main.binding = {
            alt-h = "focus left";
            alt-j = "focus down";
            alt-k = "focus up";
            alt-l = "focus right";
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."aerospace/aerospace.toml".source =
      settingsFormat.generate "aerospace.toml" cfg.settings;
  };
}
