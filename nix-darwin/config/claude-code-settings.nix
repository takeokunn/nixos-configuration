{
  pkgs,
  username,
  guardAndGuide,
  ...
}:
let
  settings = import ../../shared/claude-code-managed-settings.nix {
    inherit (pkgs) lib;
    inherit guardAndGuide pkgs;
    homeDirectory = "/Users/${username}";
  };

  settingsJson = (pkgs.formats.json { }).generate "claude-code-managed-settings.json" settings;

  managedSettingsDir = "/Library/Application Support/ClaudeCode";
in
{
  # `extraActivation` runs near the front of the single `set -e` activation script, so less can fail
  # ahead of this install (see nix-darwin/default.nix). Sharing the slot with `wallpaper.nix` is
  # harmless: `text` is `types.lines`, so fragments concatenate.
  system.activationScripts.extraActivation.text = ''
    mkdir -p "${managedSettingsDir}"
    install -m 0644 "${settingsJson}" "${managedSettingsDir}/managed-settings.json"
  '';
}
