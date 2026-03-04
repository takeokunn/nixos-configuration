{
  config,
  pkgs,
  lib ? pkgs.lib,
  ...
}:
let
  isDarwin = pkgs.stdenv.isDarwin;

  cfg = config.services.lmstudio;
in
{
  options.services.lmstudio = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable LM Studio service (Darwin only)";
    };

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = pkgs.lmstudio;
      description = "LM Studio package to use";
    };

    environmentVariables = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Environment variables for LM Studio";
    };
  };

  config = lib.mkIf (cfg.enable && isDarwin) {
    # Add LM Studio package to home.packages
    home.packages = [ cfg.package ];

    # Configure launchd agent for LM Studio
    launchd.agents.lmstudio = {
      enable = true;
      config = {
        Label = "org.nix-community.home.lmstudio";
        ProgramArguments = [
          "${cfg.package}/Applications/LM Studio.app/Contents/MacOS/LM Studio"
        ];
        RunAtLoad = true;
        KeepAlive = {
          SuccessfulExit = false;
        };
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/lmstudio/stdout.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/lmstudio/stderr.log";
        WorkingDirectory = "${config.home.homeDirectory}";

        # Configure LM Studio to expose OpenAI-compatible API on localhost:1234
        EnvironmentVariables = lib.mkMerge [
          {
            # Default environment for LM Studio
            LMS_API_PORT = "1234";
            LMS_API_HOST = "127.0.0.1";
          }
          cfg.environmentVariables
        ];
      };
    };

    # Create log directory
    home.activation.createLMStudioLogsDir = {
      after = [ "writeBoundary" ];
      before = [ ];
      data = ''
        mkdir -p ${config.home.homeDirectory}/Library/Logs/lmstudio
      '';
    };

    # Gracefully stop LM Studio before setupLaunchAgents
    #
    # Stopping the agent before setupLaunchAgents is critical because:
    # 1. setupLaunchAgents will write the new plist file atomically
    # 2. If the agent is running when the plist is replaced, macOS may not notice the change
    # 3. Some launchctl operations fail on services that are mid-transition
    # 4. Ensures clean state for the restart phase that follows setupLaunchAgents
    home.activation.preStopLMStudio = {
      after = [ "writeBoundary" ];
      before = [ "setupLaunchAgents" ];
      data = ''
        AGENT_NAME="org.nix-community.home.lmstudio"
        PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
        DOMAIN="gui/$UID"

        if [[ -f "$PLIST_PATH" ]]; then
          NEW_PLIST="$newGenPath/LaunchAgents/$AGENT_NAME.plist"
          # cmp -s performs a silent byte-by-byte comparison of the plists
          # This avoids unnecessary stop/start cycles when only Home Manager metadata changes
          # (e.g., timestamps or generation paths) while the actual configuration is identical
          if ! { [[ -f "$NEW_PLIST" ]] && cmp -s "$NEW_PLIST" "$PLIST_PATH"; }; then
            if /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
              noteEcho "Stopping LM Studio before setupLaunchAgents"
              run /bin/launchctl bootout "$DOMAIN/$AGENT_NAME" || true
              # Wait up to 15 seconds for the service to fully stop
              # launchctl bootout is asynchronous; the service may still be in "stopping" state
              # This loop prevents race conditions where we try to restart a service that hasn't fully stopped yet
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

    # Restart LM Studio after setupLaunchAgents
    home.activation.restartLMStudio = {
      after = [ "setupLaunchAgents" ];
      before = [ ];
      data = ''
        AGENT_NAME="org.nix-community.home.lmstudio"
        PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
        DOMAIN="gui/$UID"

        if [[ -f "$PLIST_PATH" ]]; then
          if /bin/launchctl print "$DOMAIN/$AGENT_NAME" >/dev/null 2>&1; then
            # Try kickstart first: it restarts the service in-place by sending a SIGKILL then reloading
            # This is faster and more graceful than bootout/bootstrap for services that are already running
            # It preserves the service state and is less disruptive to dependent processes
            if run /bin/launchctl kickstart -k "$DOMAIN/$AGENT_NAME"; then
              noteEcho "LM Studio restarted via kickstart"
            else
              # Fallback to bootout/bootstrap cycle if kickstart fails
              # This happens when the service is in an inconsistent state or launchd has lost track of it
              # bootout removes the service from launchd, then bootstrap re-registers it
              # This is more reliable but slower as it requires a full service lifecycle restart
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
  };
}
