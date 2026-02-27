{
  pkgs,
  lib ? pkgs.lib,
  emacsPkg,
  ...
}:
let
  isDarwin = pkgs.stdenv.isDarwin;
  # Use the emacsWithPackages derivation (before the open-a-Emacs wrapper replaces
  # Emacs.app/Contents/MacOS/Emacs with an emacsclient script).  The daemon must
  # use the real Cocoa binary so NSApp is initialised with proper bundle context,
  # allowing emacsclient -c to create GUI frames.
  cocoaEmacs =
    if isDarwin && emacsPkg ? passthru && emacsPkg.passthru ? withPackages
    then emacsPkg.passthru.withPackages
    else emacsPkg;
in
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  # macOS: Set TMPDIR so emacs daemon creates socket in /tmp
  launchd.agents.emacs.config.EnvironmentVariables = lib.mkIf isDarwin {
    TMPDIR = "/tmp";
  };

  # macOS: Launch daemon via Emacs.app binary directly, not the bin/emacs shell
  # wrapper.  The shell wrapper lacks .app bundle context, so NSApp does not
  # initialise properly and emacsclient -c cannot create GUI frames.
  launchd.agents.emacs.config.ProgramArguments = lib.mkIf isDarwin (lib.mkForce [
    "${cocoaEmacs}/Applications/Emacs.app/Contents/MacOS/Emacs"
    "--fg-daemon"
  ]);

  # macOS: Gracefully stop Emacs before setupLaunchAgents to prevent I/O error 5
  # (upstream bootoutAgent sleeps only 1s, insufficient for Emacs shutdown)
  home.activation.preStopEmacsDaemon = lib.mkIf isDarwin {
    after = [ "writeBoundary" ];
    before = [ "setupLaunchAgents" ];
    data = ''
      AGENT_NAME="org.nix-community.home.emacs"
      PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
      DOMAIN="gui/$UID"

      if [[ -f "$PLIST_PATH" ]]; then
        NEW_PLIST="$newGenPath/LaunchAgents/$AGENT_NAME.plist"
        if ! { [[ -f "$NEW_PLIST" ]] && cmp -s "$NEW_PLIST" "$PLIST_PATH"; }; then
          if /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
            noteEcho "Pre-stopping Emacs daemon before setupLaunchAgents"
            run ${pkgs.coreutils}/bin/timeout 5 \
              ${emacsPkg}/bin/emacsclient -e '(kill-emacs)' >/dev/null 2>&1 || true
            run /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" || true
            for i in $(seq 1 15); do
              if ! /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
                break
              fi
              sleep 1
            done
          fi
        fi
      fi
    '';
  };

  # macOS: Restart Emacs daemon after setupLaunchAgents to pick up config changes
  home.activation.restartEmacsDaemon = lib.mkIf isDarwin {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      AGENT_NAME="org.nix-community.home.emacs"
      PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
      DOMAIN="gui/$UID"

      if [[ -f "$PLIST_PATH" ]]; then
        if /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
          if run /bin/launchctl kickstart -k "$DOMAIN/$AGENT_NAME"; then
            noteEcho "Emacs daemon restarted via kickstart"
          else
            noteEcho "Kickstart failed; performing bootout/bootstrap cycle"
            run /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" || true
            for i in $(seq 1 15); do
              if ! /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
                break
              fi
              sleep 1
            done
            run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" || {
              warnEcho "Bootstrap failed — check: launchctl print gui/\$UID/$AGENT_NAME"
            }
          fi
        else
          run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" || {
            warnEcho "Bootstrap failed — check: launchctl print gui/\$UID/$AGENT_NAME"
          }
        fi
      fi
    '';
  };
}
