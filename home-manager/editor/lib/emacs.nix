{
  pkgs,
  emacsPkg,
}:
let
  constants = import ./emacs-constants.nix;
  inherit (constants)
    defaultWindowWidth
    defaultWindowHeight
    defaultAppId
    scratchpadInstanceGroup
    ;
  socketPath =
    if pkgs.stdenv.hostPlatform.isDarwin then
      constants.socketPath
    else
      "/run/user/$(id -u)/emacs/server";

  # Holds the single-instance socket open with no window so the hotkey pays
  # only a window create, not a kitty cold start. Close confirmation is a
  # process-wide setting, so it must be disabled here rather than per window.
  scratchpadKittyServer = pkgs.writeShellScript "emacs-scratchpad-kitty-server" ''
    exec ${pkgs.kitty}/bin/kitty \
      --single-instance \
      --instance-group ${scratchpadInstanceGroup} \
      --start-as=hidden \
      -o confirm_os_window_close=0 \
      -o macos_quit_when_last_window_closed=no \
      -o remember_window_size=no \
      -- ${pkgs.coreutils}/bin/true
  '';

  mkScratchpadToggle =
    {
      windowManager,
      windowWidth ? defaultWindowWidth,
      windowHeight ? defaultWindowHeight,
      appId ? defaultAppId,
    }:
    assert
      windowManager == "aerospace"
      || windowManager == "niri"
      || throw "windowManager must be 'aerospace' or 'niri', got: ${windowManager}";
    let
      emacsclient = "${emacsPkg}/bin/emacsclient";
      kitty = "${pkgs.kitty}/bin/kitty";
      jq = "${pkgs.jq}/bin/jq";
      niri = "${pkgs.niri}/bin/niri";
      emacsclientTerminal = pkgs.writeShellScript "emacs-scratchpad-emacsclient" ''
        EMACSCLIENT="${emacsclient}"
        SOCKET="${socketPath}"

        open_scratchpad() {
          "$EMACSCLIENT" -s "$SOCKET" -t -e "(my/scratchpad-init)"
        }

        # The service manager keeps the daemon running, so the socket normally exists and
        # emacsclient connects without a readiness probe.
        if [ -S "$SOCKET" ] && open_scratchpad; then
          exit 0
        fi

        # The daemon is starting, or crashed and left a socket that refuses connections.
        ${
          if pkgs.stdenv.hostPlatform.isDarwin then
            ''/bin/launchctl kickstart "gui/$UID/org.nix-community.home.emacs" >/dev/null 2>&1''
          else
            "${pkgs.systemd}/bin/systemctl --user start emacs.service >/dev/null 2>&1"
        }
        i=0
        until "$EMACSCLIENT" -s "$SOCKET" -e t >/dev/null 2>&1 || [ "$i" -ge 100 ]; do
          i=$((i + 1))
          sleep 0.1
        done
        open_scratchpad
      '';

      lockFunctions = ''
        acquire_lock() {
          if mkdir "$LOCK_DIR" 2>/dev/null; then
            printf '%s\n' "$$" > "$LOCK_DIR/pid"
            return 0
          fi

          local old_pid
          if [ -r "$LOCK_DIR/pid" ]; then
            old_pid="$(${pkgs.coreutils}/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)"
            if [ -n "$old_pid" ] && ! kill -0 "$old_pid" 2>/dev/null; then
              ${pkgs.coreutils}/bin/rm -f "$LOCK_DIR/pid"
              rmdir "$LOCK_DIR" 2>/dev/null || true
              if mkdir "$LOCK_DIR" 2>/dev/null; then
                printf '%s\n' "$$" > "$LOCK_DIR/pid"
                return 0
              fi
            fi
          fi

          return 1
        }

        release_lock() {
          if [ -f "$LOCK_DIR/pid" ] && [ "$(${pkgs.coreutils}/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)" = "$$" ]; then
            ${pkgs.coreutils}/bin/rm -f "$LOCK_DIR/pid"
            rmdir "$LOCK_DIR" 2>/dev/null || true
          fi
        }
      '';

      aerospaceScript = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
        APP_TITLE="${appId}"
        AEROSPACE="/run/current-system/sw/bin/aerospace"
        KITTY="${kitty}"
        LOCK_DIR="''${TMPDIR:-/tmp}/emacs-scratchpad-$APP_TITLE.lock"

        window_id_by_title() {
          local id title
          while IFS='|' read -r id title; do
            if [[ "$title" == *"$APP_TITLE"* ]]; then
              printf '%s' "$id"
              return
            fi
          done < <("$AEROSPACE" list-windows --all --format '%{window-id}|%{window-title}')
        }

        focused_window_id() {
          "$AEROSPACE" list-windows --focused --format '%{window-id}'
        }

        ${lockFunctions}

        focus_existing_window() {
          local i=0
          while [ "$i" -lt 500 ]; do
            TARGET_ID="$(window_id_by_title)"
            if [ -n "$TARGET_ID" ]; then
              "$AEROSPACE" focus --window-id "$TARGET_ID"
              return 0
            fi
            i=$((i + 1))
            sleep 0.02
          done

          return 1
        }

        focus_or_toggle_window() {
          local target_id="$1"
          local focused_id
          focused_id="$(focused_window_id)"

          if [ -n "$focused_id" ] && [ "$focused_id" = "$target_id" ]; then
            "$AEROSPACE" focus-back-and-forth
          else
            "$AEROSPACE" focus --window-id "$target_id"
          fi
        }

        TARGET_ID="$(window_id_by_title)"

        if [ -n "$TARGET_ID" ]; then
          focus_or_toggle_window "$TARGET_ID"
          exit 0
        fi

        if ! acquire_lock; then
          exit 0
        fi
        trap 'release_lock' EXIT INT TERM

        TARGET_ID="$(window_id_by_title)"
        if [ -n "$TARGET_ID" ]; then
          focus_or_toggle_window "$TARGET_ID"
          exit 0
        fi

        # Keep hotkey startup on AeroSpace/kitty/emacsclient only. AppleScript/System Events
        # adds a fixed delay and can race with Accessibility permissions during login.
        # The instance group is normally served by the resident kitty that scratchpadKittyServer
        # (started by home-manager/editor/emacs-scratchpad at login) holds open; when it is
        # absent this invocation becomes the server and must not quit on close.
        "$KITTY" \
          --single-instance \
          --instance-group ${scratchpadInstanceGroup} \
          -o close_on_child_death=yes \
          -o confirm_os_window_close=0 \
          -o macos_quit_when_last_window_closed=no \
          -o remember_window_size=no \
          -o initial_window_width=${toString windowWidth} \
          -o initial_window_height=${toString windowHeight} \
          -T "$APP_TITLE" \
          -- ${emacsclientTerminal} &

        focus_existing_window
      '';

      niriScript = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
        APP_ID="${appId}"
        LOCK_DIR="''${XDG_RUNTIME_DIR:-/tmp}/emacs-scratchpad-$APP_ID.lock"

        window_data() {
          ${niri} msg -j windows | ${jq} -r --arg id "$APP_ID" '
            first(.[] | select(.app_id == $id) | "\(.id) \(.is_focused)") // empty
          '
        }

        ${lockFunctions}

        center_new_window() {
          local i=0
          while [ "$i" -lt 500 ]; do
            window_data_value="$(window_data)"
            if [ -n "$window_data_value" ]; then
              ${niri} msg action center-window
              return 0
            fi
            i=$((i + 1))
            sleep 0.02
          done

          return 1
        }

        window_data_value="$(window_data)"

        if [ -z "$window_data_value" ]; then
          if ! acquire_lock; then
            exit 0
          fi
          trap 'release_lock' EXIT INT TERM

          window_data_value="$(window_data)"
          if [ -z "$window_data_value" ]; then
            XMODIFIERS=@im= ${kitty} --single-instance --instance-group ${scratchpadInstanceGroup} --class "$APP_ID" -o confirm_os_window_close=0 -o initial_window_width=80c -o initial_window_height=24c -e ${emacsclientTerminal} &
            center_new_window
          fi
        fi

        if [ -n "$window_data_value" ]; then
          window_id="''${window_data_value%% *}"
          is_focused="''${window_data_value#* }"

          if [ "$is_focused" = "true" ]; then
            ${niri} msg action focus-window-previous
          else
            ${niri} msg action focus-window --id "$window_id"
          fi
        fi
      '';
    in
    if windowManager == "aerospace" then aerospaceScript else niriScript;
in
{
  inherit
    socketPath
    mkScratchpadToggle
    scratchpadKittyServer
    defaultWindowWidth
    defaultWindowHeight
    ;
}
