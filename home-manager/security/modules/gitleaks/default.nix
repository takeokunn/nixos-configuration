{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.gitleaks;
  tomlFormat = pkgs.formats.toml { };
in
{
  options.programs.gitleaks = {
    enable = lib.mkEnableOption "Scan git repos for secrets";
    package = lib.mkPackageOption pkgs "gitleaks" { };
    enableBashIntegration = lib.hm.shell.mkBashIntegrationOption { inherit config; };
    enableZshIntegration = lib.hm.shell.mkZshIntegrationOption { inherit config; };
    enableFishIntegration = lib.hm.shell.mkFishIntegrationOption { inherit config; };
    settings = lib.mkOption {
      inherit (tomlFormat) type;
      default = { };
      description = "Contents of the gitleaks config.toml, in gitleaks' own schema.";
      example = lib.literalExpression ''
        {
          extend.useDefault = true;
          allowlists = [ { paths = [ "test/.*" ]; } ];
        }
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."gitleaks/config.toml".source =
      tomlFormat.generate "gitleaks-config.toml" cfg.settings;
    home.sessionVariables.GITLEAKS_CONFIG = "${config.xdg.configHome}/gitleaks/config.toml";
    programs.bash.initExtra = lib.mkIf cfg.enableBashIntegration ''
      eval "$(${lib.getExe cfg.package} completion bash)"
    '';
    programs.zsh.initContent = lib.mkIf cfg.enableZshIntegration ''
      eval "$(${lib.getExe cfg.package} completion zsh)"
    '';
    programs.fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} completion fish | source
    '';
  };
}
