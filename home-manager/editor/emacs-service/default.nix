{
  pkgs,
  emacsPkg,
  nurPkgs,
  ...
}:
let
  lib = pkgs.lib;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  emacsSocketPath = (import ../lib/emacs-constants.nix).socketPath;
  emacsLaunchdCommon = ''
    AGENT_NAME="org.nix-community.home.emacs"
    PLIST_PATH="$HOME/Library/LaunchAgents/$AGENT_NAME.plist"
    DOMAIN="gui/$UID"
    LEGACY_DOMAIN="user/$UID"
    EMACSCLIENT="${emacsPkg}/bin/emacsclient"
    EMACS_SOCKET="${emacsSocketPath}"

    emacs_daemon_alive() {
      "$EMACSCLIENT" -s "$EMACS_SOCKET" -e '(emacs-pid)' >/dev/null 2>&1
    }

    emacs_daemon_stopped() {
      ! emacs_daemon_alive
    }

    launchd_agent_loaded_in() {
      local domain="$1"
      /bin/launchctl print "$domain/$AGENT_NAME" >/dev/null 2>&1
    }

    launchd_agent_unloaded_in() {
      local domain="$1"
      ! launchd_agent_loaded_in "$domain"
    }

    launchd_agent_loaded() {
      launchd_agent_loaded_in "$DOMAIN"
    }

    launchd_agent_unloaded() {
      launchd_agent_unloaded_in "$DOMAIN"
    }

    legacy_launchd_agent_loaded() {
      launchd_agent_loaded_in "$LEGACY_DOMAIN"
    }

    legacy_launchd_agent_unloaded() {
      launchd_agent_unloaded_in "$LEGACY_DOMAIN"
    }

    emacs_fully_stopped() {
      launchd_agent_unloaded && legacy_launchd_agent_unloaded && emacs_daemon_stopped
    }

    wait_until() {
      local predicate="$1"
      local warning="$2"

      for i in $(${pkgs.coreutils}/bin/seq 1 15); do
        if "$predicate"; then
          return 0
        fi
        sleep 1
      done

      warnEcho "$warning"
      return 1
    }

    stop_emacs_server() {
      run ${pkgs.coreutils}/bin/timeout 5 \
        "$EMACSCLIENT" -s "$EMACS_SOCKET" -e '(kill-emacs)' >/dev/null 2>&1 || true
    }

    bootout_emacs_agent() {
      local domain="$1"
      if launchd_agent_loaded_in "$domain"; then
        run /bin/launchctl bootout "$domain/$AGENT_NAME" || true
      fi
    }

    bootout_target_emacs_agent() {
      bootout_emacs_agent "$DOMAIN"
    }

    bootout_legacy_emacs_agent() {
      bootout_emacs_agent "$LEGACY_DOMAIN"
    }
  '';
  # Use the emacsWithPackages derivation (before the open-a-Emacs wrapper replaces
  # Emacs.app/Contents/MacOS/Emacs with an emacsclient script).  The daemon must
  # use the real Cocoa binary so NSApp is initialised with proper bundle context,
  # allowing emacsclient -c to create GUI frames.
  cocoaEmacs =
    if isDarwin && emacsPkg ? passthru && emacsPkg.passthru ? withPackages then
      emacsPkg.passthru.withPackages
    else
      emacsPkg;
in
{
  services.emacs.enable = true;
  services.emacs.package = emacsPkg;
  services.emacs.client.enable = true;
  services.emacs.startWithUserSession = lib.mkIf isDarwin "graphical";

  # Linux: start emacs daemon after WAYLAND_DISPLAY is available in the systemd
  # user environment (niri exports it to systemd via dbus-update-activation-environment
  # as part of graphical-session.target activation).
  systemd.user.services.emacs = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    Unit.After = [ "graphical-session.target" ];
    Unit.PartOf = [ "graphical-session.target" ];
    Service.Restart = lib.mkForce "always";
    Service.RestartSec = 10;
  };

  launchd.agents.emacs = lib.mkIf isDarwin {
    domain = lib.mkForce "gui";
    config = {
      EnvironmentVariables = {
        # Make the daemon create its socket in /tmp.
        TMPDIR = "/tmp";
        # launchd passes no TERMINFO_DIRS, and Emacs's ncurses lacks xterm-kitty, so
        # `emacsclient -t` from kitty (the scratchpad) fails without kitty's entry.
        # The trailing empty entry keeps ncurses' compiled-in database searched.
        TERMINFO_DIRS = "${pkgs.kitty.terminfo}/share/terminfo:";
      };

      # Launch the Emacs.app binary directly, not the bin/emacs shell wrapper. The
      # wrapper lacks .app bundle context, so NSApp does not initialise properly and
      # emacsclient -c cannot create GUI frames.
      ProgramArguments = lib.mkForce [
        "${cocoaEmacs}/Applications/Emacs.app/Contents/MacOS/Emacs"
        "--fg-daemon"
      ];
      KeepAlive = lib.mkForce true;
      ThrottleInterval = 10;

      # launchd's default 256-file soft rlimit is too low: native-comp .eln files alone keep
      # ~500 FDs open at idle, so Emacs hits "too many open files". Set it on the agent so it
      # does not depend on the global launchctl limit (a separate launchd domain).
      SoftResourceLimits.NumberOfFiles = 65536;
      HardResourceLimits.NumberOfFiles = 65536;
    };
  };

  # kuro-module.el loads its native module from ~/.local/share/kuro/ only when the file is
  # mode 0600, has a single hard link, and is not a symlink. A store path fails all three,
  # so copy it with `install -m 600`, the layout upstream's installer produces.
  home.activation.installKuroModule =
    let
      libName = if pkgs.stdenv.hostPlatform.isDarwin then "libkuro_core.dylib" else "libkuro_core.so";
    in
    {
      after = [ "writeBoundary" ];
      before = [ ];
      data = ''
        run mkdir -p "$HOME/.local/share/kuro"
        run install -m 600 "${nurPkgs.kuro}/lib/${libName}" "$HOME/.local/share/kuro/${libName}"
      '';
    };

  # macOS: Gracefully stop Emacs before setupLaunchAgents to prevent I/O error 5
  # (upstream bootoutAgent sleeps only 1s, insufficient for Emacs shutdown)
  home.activation.preStopEmacsDaemon = lib.mkIf isDarwin {
    after = [ "writeBoundary" ];
    before = [ "setupLaunchAgents" ];
    data = ''
      ${emacsLaunchdCommon}

      stop_emacs_daemon() {
        if ! launchd_agent_loaded && ! legacy_launchd_agent_loaded && ! emacs_daemon_alive; then
          return 0
        fi

        noteEcho "Pre-stopping Emacs daemon before setupLaunchAgents"
        stop_emacs_server
        bootout_target_emacs_agent
        bootout_legacy_emacs_agent

        wait_until emacs_fully_stopped "Emacs daemon did not stop within 15 seconds"
      }

      if [[ -f "$PLIST_PATH" ]]; then
        NEW_PLIST="$newGenPath/LaunchAgents/$AGENT_NAME.plist"
        if ! { [[ -f "$NEW_PLIST" ]] && cmp -s "$NEW_PLIST" "$PLIST_PATH"; }; then
          stop_emacs_daemon
        fi
      fi
    '';
  };

  # macOS: Restart Emacs daemon after setupLaunchAgents to pick up config changes
  home.activation.restartEmacsDaemon = lib.mkIf isDarwin {
    after = [ "setupLaunchAgents" ];
    before = [ ];
    data = ''
      ${emacsLaunchdCommon}

      stop_conflicting_emacs_daemon() {
        if launchd_agent_loaded; then
          return 0
        fi

        if ! legacy_launchd_agent_loaded && ! emacs_daemon_alive; then
          return 0
        fi

        noteEcho "Stopping conflicting Emacs daemon before launchd bootstrap"
        bootout_legacy_emacs_agent
        stop_emacs_server
        wait_until emacs_fully_stopped "Conflicting Emacs daemon did not stop within 15 seconds"
      }

      bootstrap_emacs_agent() {
        stop_conflicting_emacs_daemon
        run /bin/launchctl bootstrap "$DOMAIN" "$PLIST_PATH" || {
          warnEcho "Bootstrap failed; check: launchctl print gui/\$UID/$AGENT_NAME"
        }
      }

      restart_loaded_emacs_agent() {
        if run /bin/launchctl kickstart -k "$DOMAIN/$AGENT_NAME"; then
          noteEcho "Emacs daemon restarted via kickstart"
          return 0
        fi

        noteEcho "Kickstart failed; performing bootout/bootstrap cycle"
        bootout_target_emacs_agent
        wait_until launchd_agent_unloaded "Emacs launchd agent did not unload within 15 seconds"
        bootstrap_emacs_agent
      }

      if [[ -f "$PLIST_PATH" ]]; then
        if launchd_agent_loaded; then
          restart_loaded_emacs_agent
        else
          bootstrap_emacs_agent
        fi
      fi
    '';
  };
}
