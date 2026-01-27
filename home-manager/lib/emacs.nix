{
  lib,
  pkgs,
  emacsPkg,
}:
let
  # Import constants (single source of truth)
  constants = import ./emacs-constants.nix;
  inherit (constants)
    socketPath
    defaultWindowWidth
    defaultWindowHeight
    defaultAppId
    ;

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
            --single-instance=no \
            -o close_on_child_death=yes \
            -o macos_quit_when_last_window_closed=yes \
            -o term=xterm-256color \
            -o focus_reporting_protocol=none \
            -o remember_window_size=no \
            -o initial_window_width=${toString windowWidth} \
            -o initial_window_height=${toString windowHeight} \
            -T "$APP_TITLE" \
            -- "$EMACSCLIENT" -s "$SOCKET" -t -e "(my/scratchpad-init)" &

          # Wait for window and center it using NSScreen.visibleFrame for accurate positioning
          sleep 0.5
          WIN_W=${toString windowWidth}
          WIN_H=${toString windowHeight}
          APP_ID_FOR_SCRIPT="${appId}"
          osascript <<APPLESCRIPT
        use framework "AppKit"
        use scripting additions

        set mainScreen to current application's NSScreen's mainScreen()
        set visFrame to mainScreen's visibleFrame()
        set fullFrame to mainScreen's frame()

        set visOrigin to item 1 of visFrame
        set visSize to item 2 of visFrame
        set fullSize to item 2 of fullFrame

        set screenX to (item 1 of visOrigin) as integer
        set screenW to (item 1 of visSize) as integer
        set screenH to (item 2 of visSize) as integer
        set fullH to (item 2 of fullSize) as integer

        -- Convert from NSScreen coords (bottom-left origin) to window coords (top-left origin)
        set screenY to (fullH - (item 2 of visOrigin) - screenH) as integer

        set winW to $WIN_W
        set winH to $WIN_H
        set posX to screenX + ((screenW - winW) / 2) as integer
        set posY to screenY + ((screenH - winH) / 2) as integer

        tell application "System Events"
            tell process "kitty"
                repeat with w in windows
                    if name of w contains "$APP_ID_FOR_SCRIPT" then
                        set position of w to {posX, posY}
                        set size of w to {winW, winH}
                        exit repeat
                    end if
                end repeat
            end tell
        end tell
        APPLESCRIPT
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
  inherit
    socketPath
    mkScratchpadToggle
    defaultWindowWidth
    defaultWindowHeight
    ;
}
