{
  pkgs,
  lib ? pkgs.lib,
  emacsPkg,
  ...
}:
let
  isDarwin = pkgs.stdenv.isDarwin;
in
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  # macOS: Set TMPDIR for launchd agent so emacs daemon creates socket in /tmp
  launchd.agents.emacs.config.EnvironmentVariables = lib.mkIf isDarwin {
    TMPDIR = "/tmp";
  };

  # macOS: Restart Emacs daemon on activation to pick up config changes
  # Uses kickstart -k for atomic restart, avoiding race condition with setupLaunchAgents
  home.activation.restartEmacsDaemon = lib.mkIf isDarwin {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      AGENT_NAME="org.nix-community.home.emacs"
      PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
      DOMAIN="gui/$UID"

      if [[ -f "$PLIST_PATH" ]]; then
        # Use kickstart -k for atomic restart (kills existing and restarts)
        # This avoids race condition with setupLaunchAgents which may have just started the daemon
        if /bin/launchctl kickstart -k "$DOMAIN/$AGENT_NAME" 2>/dev/null; then
          noteEcho "Emacs daemon restarted successfully"
        else
          # If kickstart fails (service not loaded), try bootstrap
          run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" 2>/dev/null || true
          noteEcho "Emacs daemon bootstrapped"
        fi

        # Verify daemon is running
        sleep 1
        if ! /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
          warnEcho "Emacs daemon may not have started - check manually with: launchctl print gui/\$UID/$AGENT_NAME"
        fi
      fi
    '';
  };
}
