{ pkgs, ... }:
let
  # Emacs Scratchpad Toggle Script for macOS
  # Similar to NixOS niri version, but uses aerospace commands
  # Window size for floating Emacs scratchpad
  windowWidth = 600;
  windowHeight = 400;

  emacsScratchpadToggle = pkgs.writeShellScript "emacs-scratchpad-toggle" ''
    APP_TITLE=FloatingEmacs
    AEROSPACE="/run/current-system/sw/bin/aerospace"
    JQ="${pkgs.jq}/bin/jq"
    KITTY="${pkgs.kitty}/bin/kitty"
    EMACSCLIENT="${pkgs.emacs}/bin/emacsclient"
    SOCKET="/tmp/emacs$(id -u)/server"

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
              if name of w contains "FloatingEmacs" then
                set position of w to {posX, posY}
                set size of w to {winW, winH}
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
in
{
  services.aerospace = {
    enable = true;
    settings = {
      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;
      accordion-padding = 0;
      on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
      exec-on-workspace-change = [
        "/bin/bash"
        "-c"
        "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(/run/current-system/sw/bin/aerospace list-workspaces --focused)"
      ];

      gaps = {
        inner = {
          horizontal = 0;
          vertical = 0;
        };
        outer = {
          left = 0;
          bottom = 0;
          top = 10;
          right = 0;
        };
      };

      mode = {
        main = {
          binding = {
            alt-h = "focus left";
            alt-l = "focus right";

            alt-shift-h = "move left";
            alt-shift-l = "move right";

            alt-shift-space = "layout floating tiling";

            alt-1 = "workspace 1";
            alt-2 = "workspace 2";
            alt-3 = "workspace 3";
            alt-4 = "workspace 4";
            alt-5 = "workspace 5";
            alt-6 = "workspace 6";
            alt-7 = "workspace 7";
            alt-8 = "workspace 8";
            alt-9 = "workspace 9";
            alt-0 = "workspace 10";

            alt-shift-1 = [
              "move-node-to-workspace 1"
              "workspace 1"
            ];
            alt-shift-2 = [
              "move-node-to-workspace 2"
              "workspace 2"
            ];
            alt-shift-3 = [
              "move-node-to-workspace 3"
              "workspace 3"
            ];
            alt-shift-4 = [
              "move-node-to-workspace 4"
              "workspace 4"
            ];
            alt-shift-5 = [
              "move-node-to-workspace 5"
              "workspace 5"
            ];
            alt-shift-6 = [
              "move-node-to-workspace 6"
              "workspace 6"
            ];
            alt-shift-7 = [
              "move-node-to-workspace 7"
              "workspace 7"
            ];
            alt-shift-8 = [
              "move-node-to-workspace 8"
              "workspace 8"
            ];
            alt-shift-9 = [
              "move-node-to-workspace 9"
              "workspace 9"
            ];
            alt-shift-0 = [
              "move-node-to-workspace 10"
              "workspace 10"
            ];

            alt-r = "mode resize";

            # Emacs Scratchpad Toggle (like NixOS Mod+I)
            alt-i = "exec-and-forget ${emacsScratchpadToggle}";
          };
        };

        resize = {
          binding = {
            h = "resize width -50";
            j = "resize height +50";
            k = "resize height -50";
            l = "resize width +50";
            enter = "mode main";
            esc = "mode main";
          };
        };
      };

      workspace-to-monitor-force-assignment = {
        "1" = "main";
        "2" = "main";
        "3" = "main";
        "4" = "main";
        "5" = "secondary";
        "6" = "main";
        "7" = "main";
        "8" = "main";
        "9" = "main";
        "10" = "main";
      };

      on-window-detected = [
        # FloatingEmacs scratchpad (kitty with specific title)
        {
          "if".app-id = "net.kovidgoyal.kitty";
          "if".window-title-regex-substring = "FloatingEmacs";
          run = [ "layout floating" ];
        }
        {
          "if".app-id = "com.google.Chrome";
          run = [
            "layout floating"
            "move-node-to-workspace 1"
          ];
        }
        {
          "if".app-id = "com.apple.Terminal";
          run = [ "move-node-to-workspace 2" ];
        }
        {
          "if".app-id = "org.gnu.Emacs";
          run = [ "move-node-to-workspace 3" ];
        }
        {
          "if".app-id = "com.jgraph.drawio.desktop";
          run = [ "move-node-to-workspace 4" ];
        }
        {
          "if".app-id = "com.lambdalisue.Arto";
          run = [ "move-node-to-workspace 4" ];
        }
        {
          "if".app-id = "com.sequel-ace.sequel-ace";
          run = [ "move-node-to-workspace 4" ];
        }
        {
          "if".app-id = "com.slite.desktop";
          run = [ "move-node-to-workspace 7" ];
        }
        {
          "if".app-id = "com.hnc.Discord";
          run = [ "move-node-to-workspace 8" ];
        }
        {
          "if".app-id = "com.clickup.desktop-app";
          run = [ "move-node-to-workspace 8" ];
        }
        {
          "if".app-id = "com.google.Chrome.app.caidcmannjgahlnbpmidmiecjcoiiigg";
          run = [ "move-node-to-workspace 9" ];
        }
        {
          "if".app-id = "com.tinyspeck.slackmacgap";
          run = [ "move-node-to-workspace 10" ];
        }
      ];
    };
  };
}
