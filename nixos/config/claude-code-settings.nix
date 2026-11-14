# Places Claude Code's managed-settings policy (shared/claude-code-managed-settings.nix) at
# /etc/claude-code/managed-settings.json, read at higher precedence than user settings. User
# preference defaults (model, effort) reach ~/.claude/settings.json through home-manager's
# programs.claude-code.mutableSettings merge, which keeps the file writable.
{
  pkgs,
  username,
  guardAndGuide,
}:
let
  settings = import ../../shared/claude-code-managed-settings.nix {
    inherit (pkgs) lib;
    inherit guardAndGuide pkgs;
    homeDirectory = "/home/${username}";
  };

  settingsFile = (pkgs.formats.json { }).generate "managed-settings.json" settings;
in
{
  environment.etc."claude-code/managed-settings.json".source = settingsFile;
}
