{ lib, pkgs, emacsPkg }:
let
  # Import constants (single source of truth)
  constants = import ./emacs-constants.nix;
  inherit (constants) socketPath defaultWindowWidth defaultWindowHeight defaultAppId;

  # Scratchpad toggle script generator
  # Generates platform-specific scripts for aerospace (macOS) and niri (NixOS)
  mkScratchpadToggle =
    {
      windowManager, # "aerospace" | "niri"
      windowWidth ? defaultWindowWidth,
      windowHeight ? defaultWindowHeight,
      appId ? defaultAppId,
    }:
    assert windowManager == "aerospace" || windowManager == "niri"
      || throw "windowManager must be 'aerospace' or 'niri', got: ${windowManager}";
    let
      emacsclient = "${emacsPkg}/bin/emacsclient";
      kitty = "${pkgs.kitty}/bin/kitty";
      jq = "${pkgs.jq}/bin/jq";

      aerospaceScript = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
        APP_TITLE=${appId}
        AEROSPACE="/run/current-system/sw/bin/aerospace"
        JQ="${jq}"
        KITTY="${kitty}"
        EMACSCLIENT="${emacsclient}"
        SOCKET="${socketPath}"

        # Get target window ID by title
        TARGET_ID=$("$AEROSPACE" list-windows --all --json | "$JQ" -r --arg title "$APP_TITLE" '
          .[] | select(.["window-title"] | contains($title)) | .["window-id"]
        ' | head -n1)

        if [ -z "$TARGET_ID" ]; then
          # No window exists - spawn new TUI Emacs in Kitty
          # Use -o term=xterm-256color because Emacs daemon doesn't have kitty's terminfo
          "$KITTY" \
            -o close_on_child_death=yes \
            -o macos_quit_when_last_window_closed=yes \
            -o term=xterm-256color \
            -o focus_reporting_protocol=none \
            -o remember_window_size=no \
            -o initial_window_width=${toString windowWidth} \
            -o initial_window_height=${toString windowHeight} \
            -T "$APP_TITLE" \
            -- "$EMACSCLIENT" -s "$SOCKET" -t -e "(my/scratchpad-init)" &

          # Wait for window and center it
          sleep 0.5
          osascript <<EOF
            tell application "Finder"
              set screenBounds to bounds of window of desktop
              set screenW to item 3 of screenBounds
              set screenH to item 4 of screenBounds
            end tell
            set winW to ${toString windowWidth}
            set winH to ${toString windowHeight}
            set posX to (screenW - winW) / 2
            set posY to (screenH - winH) / 2
            tell application "System Events"
              tell process "kitty"
                repeat with w in windows
                  if name of w contains "${appId}" then
                    set size of w to {winW, winH}
                    set position of w to {posX, posY}
                    exit repeat
                  end if
                end repeat
              end tell
            end tell
        EOF
        else
          # Get currently focused window ID
          FOCUSED_ID=$("$AEROSPACE" list-windows --focused --json | "$JQ" -r '.[0]["window-id"] // ""')

          if [ "$FOCUSED_ID" = "$TARGET_ID" ]; then
            # Already focused - switch to previous window
            "$AEROSPACE" focus-back-and-forth
          else
            # Not focused - bring to focus
            "$AEROSPACE" focus --window-id "$TARGET_ID"
          fi
        fi
      '';

      niriScript = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
        APP_ID=${appId}

        # Get window info in single IPC call
        window_data=$(${pkgs.niri}/bin/niri msg -j windows | ${jq} -r --arg id "$APP_ID" '
          .[] | select(.app_id == $id) | "\(.id) \(.is_focused)"
        ')

        if [ -z "$window_data" ]; then
          # No window exists - spawn new TUI Emacs in Kitty (centered, compact size)
          XMODIFIERS=@im= ${kitty} --class "$APP_ID" -o initial_window_width=80c -o initial_window_height=24c -e ${emacsclient} -s "${socketPath}" -t -e '(my/scratchpad-init)' &
          sleep 0.3
          # Center the newly created floating window
          ${pkgs.niri}/bin/niri msg action center-window
        else
          # Parse window data
          window_id=$(echo "$window_data" | cut -d' ' -f1)
          is_focused=$(echo "$window_data" | cut -d' ' -f2)

          if [ "$is_focused" = "true" ]; then
            # Focused - switch to previous window (pseudo-hide)
            ${pkgs.niri}/bin/niri msg action focus-window-previous
          else
            # Not focused - bring to focus
            ${pkgs.niri}/bin/niri msg action focus-window --id "$window_id"
          fi
        fi
      '';
    in
    if windowManager == "aerospace" then aerospaceScript else niriScript;
in
{
  inherit socketPath mkScratchpadToggle defaultWindowWidth defaultWindowHeight;
}
