# home-mangerが正常に機能しないので自前で文字列を生やした
# https://github.com/nix-community/home-manager/blob/master/modules/programs/zellij.nix
{
  lib,
  config,
  ...
}:
let
  cfg = config.programs.zellij;
in
with lib;
{
  options.programs.zellij = {
    rawConfig = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."zellij/config.kdl".text = config.programs.zellij.rawConfig;
  };
}
