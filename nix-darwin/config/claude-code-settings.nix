{
  pkgs,
  username,
  guardAndGuide,
}:
let
  settings = import ../../shared/claude-code-managed-settings.nix {
    inherit (pkgs) lib;
    inherit guardAndGuide;
    homeDirectory = "/Users/${username}";
  };

  settingsJson = (pkgs.formats.json { }).generate "claude-code-managed-settings.json" settings;

  managedSettingsDir = "/Library/Application Support/ClaudeCode";
in
{
  system.activationScripts.extraActivation.text = ''
    mkdir -p "${managedSettingsDir}"
    install -m 0644 "${settingsJson}" "${managedSettingsDir}/managed-settings.json"
  '';
}
