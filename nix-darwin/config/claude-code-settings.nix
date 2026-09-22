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
  # `postActivation`, not a name of this module's choosing: nix-darwin interpolates a fixed,
  # enumerated list of activation sub-scripts into `activationScripts.script.text`, so an
  # attribute under any other name defines an option that is never run and never reports it.
  # `extraActivation` is the other slot open to consumers and `wallpaper.nix` holds it; the
  # attribute is `types.unspecified`, so a second definition of the same slot would collide.
  system.activationScripts.postActivation.text = ''
    mkdir -p "${managedSettingsDir}"
    install -m 0644 "${settingsJson}" "${managedSettingsDir}/managed-settings.json"
  '';
}
