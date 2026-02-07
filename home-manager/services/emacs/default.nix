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
  # Check-first approach: inspect launchd state before choosing restart strategy
  home.activation.restartEmacsDaemon = lib.mkIf isDarwin {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      AGENT_NAME="org.nix-community.home.emacs"
      PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
      DOMAIN="gui/$UID"

      if [[ -f "$PLIST_PATH" ]]; then
        # Check whether the service is currently loaded (FR-001)
        if /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
          # Service is loaded — attempt atomic restart via kickstart -k (FR-002)
          if run /bin/launchctl kickstart -k "$DOMAIN/$AGENT_NAME"; then
            noteEcho "Emacs daemon restarted successfully via kickstart"
          else
            # Kickstart failed despite service being loaded — full cycle (FR-003)
            noteEcho "Kickstart failed; performing bootout/bootstrap cycle"
            run /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" || true

            # Poll for deregistration (up to 15s)
            for i in $(seq 1 15); do
              if ! /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
                break
              fi
              sleep 1
            done

            run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" || {
              warnEcho "Bootstrap failed after bootout — check manually with: launchctl print gui/\$UID/$AGENT_NAME"
            }
          fi
        else
          # Service is NOT loaded (FR-004)
          # Clear any residual state (bootout may legitimately fail here)
          run /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" || true
          sleep 2

          if ! run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH"; then
            # Single retry on bootstrap failure (FR-005)
            noteEcho "Bootstrap failed; retrying after delay"
            sleep 3
            run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" || {
              warnEcho "Bootstrap retry failed — check manually with: launchctl print gui/\$UID/$AGENT_NAME"
            }
          else
            noteEcho "Emacs daemon bootstrapped successfully"
          fi
        fi

        # Verify daemon started (FR-006)
        sleep 1
        if ! /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
          warnEcho "Emacs daemon may not have started — check manually with: launchctl print gui/\$UID/$AGENT_NAME"
        fi
      fi
    '';
  };
}
