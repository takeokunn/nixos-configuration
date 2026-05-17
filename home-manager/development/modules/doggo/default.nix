{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.doggo;
in
{
  options.programs.doggo = {
    enable = lib.mkEnableOption "Command-line DNS Client for Humans. Written in Golang";
    package = lib.mkPackageOption pkgs "doggo" { };

    enableBashIntegration = lib.hm.shell.mkBashIntegrationOption { inherit config; };

    enableZshIntegration = lib.hm.shell.mkZshIntegrationOption { inherit config; };

    enableFishIntegration = lib.hm.shell.mkFishIntegrationOption { inherit config; };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.bash.initExtra = lib.mkIf (cfg.enableBashIntegration && cfg.package != null) ''
      eval "$(${lib.getExe cfg.package} completions bash)"
    '';

    programs.zsh.initContent = lib.mkIf (cfg.enableZshIntegration && cfg.package != null) ''
      eval "$(${lib.getExe cfg.package} completions zsh)"
    '';

    programs.fish.interactiveShellInit = lib.mkIf (cfg.enableFishIntegration && cfg.package != null) ''
      ${lib.getExe cfg.package} completions fish | source
    '';
  };
}
