{
  lib,
  pkgs,
  emacsPkg,
}:
let
  # Import constants (single source of truth)
  constants = import ./emacs-constants.nix;
  inherit (constants)
    defaultWindowWidth
    defaultWindowHeight
    defaultAppId
    ;
  socketPath =
    if pkgs.stdenv.isDarwin then constants.socketPath else "/run/user/$(id -u)/emacs/server";

  # Scratchpad toggle script generator
  # Generates platform-specific scripts for aerospace (macOS) and niri (NixOS)
  mkScratchpadToggle =
    {
      windowManager, # "aerospace" | "niri"
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
      emacsclientTerminal = pkgs.writeShellScript "emacs-scratchpad-emacsclient" ''
        EMACSCLIENT="${emacsclient}"
        SOCKET="${socketPath}"
        TIMEOUT="${pkgs.coreutils}/bin/timeout"

        emacs_ready() {
          "$TIMEOUT" 1 "$EMACSCLIENT" -s "$SOCKET" -e '(emacs-pid)' >/dev/null 2>&1
        }

        start_emacs_service() {
          ${
            if pkgs.stdenv.isDarwin then
              ''
                /bin/launchctl kickstart "gui/$UID/org.nix-community.home.emacs" >/dev/null 2>&1 || true
              ''
            else
              ''
                ${pkgs.systemd}/bin/systemctl --user start emacs.service >/dev/null 2>&1 || true
              ''
          }
        }

        if ! emacs_ready; then
          start_emacs_service
        fi

        i=0
        while [ "$i" -lt 100 ]; do
          if emacs_ready; then
            exec "$EMACSCLIENT" -s "$SOCKET" -t -e "(my/scratchpad-init)"
          fi
          i=$((i + 1))
          sleep 0.1
        done

        printf '%s\n' "emacs daemon did not become ready for $SOCKET" >&2
        exit 1
      '';

      aerospaceScript = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
        APP_TITLE="${appId}"
        AEROSPACE="/run/current-system/sw/bin/aerospace"
        JQ="${jq}"
        KITTY="${kitty}"
        LOCK_DIR="''${TMPDIR:-/tmp}/emacs-scratchpad-$APP_TITLE.lock"

        window_id_by_title() {
          "$AEROSPACE" list-windows --all --json | "$JQ" -r --arg title "$APP_TITLE" '
            first(.[] | select((.["window-title"] // "") | contains($title)) | .["window-id"]) // empty
          '
        }

        focused_window_id() {
          "$AEROSPACE" list-windows --focused --json | "$JQ" -r '.[0]["window-id"] // empty'
        }

        start_emacs_service() {
          /bin/launchctl kickstart "gui/$UID/org.nix-community.home.emacs" >/dev/null 2>&1 || true
        }

        acquire_lock() {
          if mkdir "$LOCK_DIR" 2>/dev/null; then
            printf '%s\n' "$$" > "$LOCK_DIR/pid"
            return 0
          fi

          local old_pid
          if [ -r "$LOCK_DIR/pid" ]; then
            old_pid="$(/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)"
            if [ -n "$old_pid" ] && ! kill -0 "$old_pid" 2>/dev/null; then
              /bin/rm -f "$LOCK_DIR/pid"
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
          if [ -f "$LOCK_DIR/pid" ] && [ "$(/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)" = "$$" ]; then
            /bin/rm -f "$LOCK_DIR/pid"
            rmdir "$LOCK_DIR" 2>/dev/null || true
          fi
        }

        focus_existing_window() {
          local i=0
          while [ "$i" -lt 100 ]; do
            TARGET_ID="$(window_id_by_title)"
            if [ -n "$TARGET_ID" ]; then
              "$AEROSPACE" focus --window-id "$TARGET_ID"
              return 0
            fi
            i=$((i + 1))
            sleep 0.1
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

        start_emacs_service

        # Keep hotkey startup on AeroSpace/kitty/emacsclient only. AppleScript/System Events
        # adds a fixed delay and can race with Accessibility permissions during login.
        "$KITTY" \
          --single-instance=no \
          -o close_on_child_death=yes \
          -o macos_quit_when_last_window_closed=yes \
          -o term=xterm-256color \
          -o focus_reporting_protocol=none \
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
          ${pkgs.niri}/bin/niri msg -j windows | ${jq} -r --arg id "$APP_ID" '
            first(.[] | select(.app_id == $id) | "\(.id) \(.is_focused)") // empty
          '
        }

        start_emacs_service() {
          ${pkgs.systemd}/bin/systemctl --user start emacs.service >/dev/null 2>&1 || true
        }

        acquire_lock() {
          if mkdir "$LOCK_DIR" 2>/dev/null; then
            printf '%s\n' "$$" > "$LOCK_DIR/pid"
            return 0
          fi

          local old_pid
          if [ -r "$LOCK_DIR/pid" ]; then
            old_pid="$(/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)"
            if [ -n "$old_pid" ] && ! kill -0 "$old_pid" 2>/dev/null; then
              /bin/rm -f "$LOCK_DIR/pid"
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
          if [ -f "$LOCK_DIR/pid" ] && [ "$(/bin/cat "$LOCK_DIR/pid" 2>/dev/null || true)" = "$$" ]; then
            /bin/rm -f "$LOCK_DIR/pid"
            rmdir "$LOCK_DIR" 2>/dev/null || true
          fi
        }

        center_new_window() {
          local i=0
          while [ "$i" -lt 100 ]; do
            window_data_value="$(window_data)"
            if [ -n "$window_data_value" ]; then
              ${pkgs.niri}/bin/niri msg action center-window
              return 0
            fi
            i=$((i + 1))
            sleep 0.1
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
            start_emacs_service
            XMODIFIERS=@im= ${kitty} --class "$APP_ID" -o initial_window_width=80c -o initial_window_height=24c -e ${emacsclientTerminal} &
            center_new_window
          fi
        fi

        if [ -n "$window_data_value" ]; then
          window_id="''${window_data_value%% *}"
          is_focused="''${window_data_value#* }"

          if [ "$is_focused" = "true" ]; then
            ${pkgs.niri}/bin/niri msg action focus-window-previous
          else
            ${pkgs.niri}/bin/niri msg action focus-window --id "$window_id"
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
    defaultWindowWidth
    defaultWindowHeight
    ;
}
