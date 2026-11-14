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
  # `extraActivation` is spliced near the front of the activation script, and the whole script is a
  # single `set -e` sequence across every slot, so the slot decides how much can fail ahead of this
  # install, not whether it is coupled to anything. Measured in the composed script: here the
  # install lands around line 940, against line 2870 from `postActivation`. Sharing the slot with
  # `wallpaper.nix` is harmless (`text` is `types.lines`, so fragments concatenate) and an
  # earlier-running neighbour that fails aborts the switch loudly rather than skipping this.
  system.activationScripts.extraActivation.text = ''
    mkdir -p "${managedSettingsDir}"
    install -m 0644 "${settingsJson}" "${managedSettingsDir}/managed-settings.json"
  '';
}
