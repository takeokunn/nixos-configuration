{ pkgs, emacsPkg, ... }:
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
    client.enable = true;
  };

  # Set TMPDIR for launchd agent so emacs daemon creates socket in /tmp
  launchd.agents.emacs.config.EnvironmentVariables = {
    TMPDIR = "/tmp";
  };

  # Always restart Emacs daemon on activation to pick up config changes
  home.activation.restartEmacsDaemon = {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      AGENT_NAME="org.nix-community.home.emacs"
      PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
      DOMAIN="gui/$UID"

      if [[ -f "$PLIST_PATH" ]]; then
        # Always attempt bootout (ignore errors if not loaded)
        /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" 2>/dev/null || true

        # Small delay to ensure clean shutdown
        sleep 1

        # Bootstrap and report any errors
        run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH"
      fi
    '';
  };
}
