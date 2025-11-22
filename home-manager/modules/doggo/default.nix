{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.doggo;
in
with lib;
{
  options.programs.doggo = {
    enable = mkEnableOption "Command-line DNS Client for Humans. Written in Golang";
    package = mkPackageOption pkgs "doggo" { };

    enableBashIntegration = lib.hm.shell.mkBashIntegrationOption { inherit config; };

    enableZshIntegration = lib.hm.shell.mkZshIntegrationOption { inherit config; };

    enableFishIntegration = lib.hm.shell.mkFishIntegrationOption { inherit config; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs = {
      bash.initExtra = mkIf (cfg.enableBashIntegration && cfg.package != null) ''
        eval "$(${getExe cfg.package} completions bash)"
      '';

      zsh.initContent = mkIf (cfg.enableZshIntegration && cfg.package != null) ''
        eval "$(${getExe cfg.package} completions zsh)"
      '';

      fish.interactiveShellInit = mkIf (cfg.enableFishIntegration && cfg.package != null) ''
        ${getExe cfg.package} completions fish | source
      '';
    };
  };
}
