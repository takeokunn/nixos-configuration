# Places Claude Code's managed-settings policy (shared/claude-code-managed-settings.nix) at
# /etc/claude-code/managed-settings.json, read at higher precedence than user settings. Home
# Manager no longer declares programs.claude-code.settings for this host: that path resolves to
# a read-only /nix/store symlink at ~/.claude/settings.json, which breaks the writes Claude Code
# itself makes there (e.g. persisting a `/model` choice).
{
  pkgs,
  inputs,
  username,
}:
let
  guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;

  settings = import ../../shared/claude-code-managed-settings.nix {
    inherit (pkgs) lib;
    inherit guardAndGuide;
    homeDirectory = "/home/${username}";
  };

  settingsFile = (pkgs.formats.json { }).generate "managed-settings.json" settings;
in
{
  environment.etc."claude-code/managed-settings.json".source = settingsFile;
}
