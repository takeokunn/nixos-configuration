{
  config,
  lib,
  pkgs,
  emacsLib,
  ...
}:
let
  emacsScratchpadToggle = emacsLib.mkScratchpadToggle {
    windowManager = "niri";
  };
  palette = config.lib.stylix.colors;
  colors = palette.withHashtag;
  rgba =
    base: alpha:
    "rgba(${palette."${base}-rgb-r"}, ${palette."${base}-rgb-g"}, ${palette."${base}-rgb-b"}, ${alpha})";

  # Called by Emacs (my/scratchpad-commit) with the text on stdin: copies it, hides the
  # scratchpad and pastes into the window that had focus before it.
  scratchpadCommit = pkgs.writeShellScriptBin "emacs-scratchpad-commit" ''
    set -eu
    ${pkgs.wl-clipboard}/bin/wl-copy
    ${pkgs.niri}/bin/niri msg action focus-window-previous
    sleep 0.1
    app_id="$(${pkgs.niri}/bin/niri msg -j focused-window | ${pkgs.jq}/bin/jq -r '.app_id // empty')"
    # Terminals take Ctrl+Shift+V; everything else takes Ctrl+V. Pasting avoids wtype's
    # keymap limit on typing many distinct characters.
    case "$app_id" in
      kitty) ${pkgs.wtype}/bin/wtype -M ctrl -M shift -k v -m shift -m ctrl ;;
      *) ${pkgs.wtype}/bin/wtype -M ctrl -k v -m ctrl ;;
    esac
  '';

  # Mod+Shift+R starts a recording and the same key stops it; SIGINT lets wl-screenrec finish the file.
  screenrecToggle = pkgs.writeShellScriptBin "screenrec-toggle" ''
    if ${pkgs.procps}/bin/pkill -INT -f wl-screenrec; then
      exit 0
    fi
    mkdir -p "$HOME/Videos/Recordings"
    exec ${pkgs.wl-screenrec}/bin/wl-screenrec -f "$HOME/Videos/Recordings/recording_$(date +%Y-%m-%d_%H-%M-%S).mp4"
  '';

  # Workspace numbers and app placement match the Mac's AeroSpace layout.
  workspaces = map toString (lib.range 1 10);
  workspaceKey = ws: if ws == "10" then "0" else ws;
  workspaceApps = {
    "1" = [ "^google-chrome$" ];
    "3" = [ "^emacs$" ];
    "9" = [ "^discord$" ];
    "10" = [ "^Slack$" ];
  };

  spring = {
    _props = {
      damping-ratio = 1.0;
      stiffness = 800;
      epsilon = 0.001;
    };
  };
in
{
  wayland.windowManager.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      input = {
        keyboard = {
          xkb = {
            layout = "jp";
            options = "ctrl:nocaps";
          };
          # Same as the Mac (InitialKeyRepeat 15, KeyRepeat 2).
          repeat-delay = 225;
          repeat-rate = 33;
        };
        touchpad = {
          tap = { };
          dwt = { };
          natural-scroll = { };
          accel-speed = 0.2;
        };
        mouse.accel-speed = 0.0;
      };

      gestures = {
        dnd-edge-workspace-switch = {
          trigger-height = 50;
          delay-ms = 100;
        };
        hot-corners.top-left = { };
      };

      layout = {
        gaps = 10;
        center-focused-column = "never";
        preset-column-widths._children = [
          { proportion = 0.33333; }
          { proportion = 0.5; }
          { proportion = 0.66667; }
        ];
        default-column-width.proportion = 0.5;
        focus-ring = {
          width = 2;
          # OKLCH hues: cyan 213, purple 302, pink 347. "shorter hue" passes purple; "longer hue"
          # would pass green (148) and yellow (113) instead.
          active-gradient._props = {
            from = colors.base0C;
            to = colors.base0E;
            angle = 135;
            "in" = "oklch shorter hue";
          };
          inactive-color = rgba "base05" "0.06";
        };
        border.off = { };
        shadow = {
          on = { };
          softness = 64;
          spread = 5;
          offset._props = {
            x = 0;
            y = 24;
          };
          draw-behind-window = true;
          # Plain black: the base16 palette has no black.
          color = "rgba(0, 0, 0, 0.5)";
          inactive-color = "rgba(0, 0, 0, 0.3)";
        };
      };

      prefer-no-csd = { };
      screenshot-path = "~/Pictures/Screenshots/Screenshot_%Y-%m-%d_%H-%M-%S.png";

      animations = {
        slowdown = 0.8;
        workspace-switch.spring._props = {
          damping-ratio = 1.0;
          stiffness = 1000;
          epsilon = 0.001;
        };
        window-open = {
          duration-ms = 150;
          curve = "ease-out-expo";
        };
        window-close = {
          duration-ms = 150;
          curve = "ease-out-quad";
        };
        horizontal-view-movement.spring = spring;
        window-movement.spring = spring;
        window-resize.spring = spring;
        config-notification-open-close.spring._props = {
          damping-ratio = 0.6;
          stiffness = 1000;
          epsilon = 0.001;
        };
      };

      hotkey-overlay.skip-at-startup = { };

      cursor = {
        xcursor-theme = config.home.pointerCursor.name;
        xcursor-size = config.home.pointerCursor.size;
      };

      environment = {
        TERM = "xterm-kitty";
        QT_QPA_PLATFORM = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        MOZ_ENABLE_WAYLAND = "1";
        XDG_CURRENT_DESKTOP = "niri";
        XDG_SESSION_TYPE = "wayland";
        XDG_SESSION_DESKTOP = "niri";
        NIXOS_OZONE_WL = "1";
      };

      binds = {
        "Mod+Return".spawn = "kitty";
        "Mod+Shift+Return".spawn = [
          "kitty"
          "-e"
          "tmux"
        ];

        "Mod+D" = {
          _props.repeat = false;
          spawn = [
            "vicinae"
            "vicinae://toggle"
          ];
        };
        "Mod+V" = {
          _props.repeat = false;
          spawn = [
            "vicinae"
            "vicinae://launch/clipboard/history?toggle=true"
          ];
        };

        # Emacs scratchpad, used as the input method; C-c C-c in it pastes back.
        "Mod+I".spawn = [
          "sh"
          "-c"
          "${emacsScratchpadToggle}"
        ];

        "Mod+P".screenshot = { };
        "Mod+Shift+P".screenshot-screen = { };
        "Mod+Ctrl+P".screenshot-window = { };
        # Region capture opened in Satty for annotation (the CleanShot replacement).
        "Mod+Shift+S".spawn-sh =
          ''${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.satty}/bin/satty --filename -'';

        "Mod+Shift+Q".close-window = { };

        "Mod+H".focus-column-left = { };
        "Mod+J".focus-window-down = { };
        "Mod+K".focus-window-up = { };
        "Mod+L".focus-column-right = { };

        "Mod+Shift+H".move-column-left = { };
        "Mod+Shift+J".move-window-down = { };
        "Mod+Shift+K".move-window-up = { };
        "Mod+Shift+L".move-column-right = { };

        "Mod+Ctrl+K".focus-workspace-up = { };
        "Mod+Ctrl+J".focus-workspace-down = { };
        "Mod+Ctrl+Shift+K".move-column-to-workspace-up = { };
        "Mod+Ctrl+Shift+J".move-column-to-workspace-down = { };

        "Mod+Alt+H".focus-monitor-left = { };
        "Mod+Alt+L".focus-monitor-right = { };
        "Mod+Alt+K".focus-monitor-up = { };
        "Mod+Alt+J".focus-monitor-down = { };
        "Mod+Alt+Shift+H".move-column-to-monitor-left = { };
        "Mod+Alt+Shift+L".move-column-to-monitor-right = { };
        "Mod+Alt+Shift+K".move-column-to-monitor-up = { };
        "Mod+Alt+Shift+J".move-column-to-monitor-down = { };

        "Mod+F".fullscreen-window = { };
        "Mod+Shift+Space".toggle-window-floating = { };

        "Mod+Minus".set-column-width = "-10%";
        "Mod+Equal".set-column-width = "+10%";
        "Mod+Shift+Minus".set-window-height = "-10%";
        "Mod+Shift+Equal".set-window-height = "+10%";

        "Mod+BracketLeft".consume-window-into-column = { };
        "Mod+BracketRight".expel-window-from-column = { };

        "Mod+R".switch-preset-column-width = { };
        "Mod+Alt+R".reset-window-height = { };
        "Mod+Ctrl+R".switch-preset-window-height = { };
        "Mod+Shift+F".maximize-column = { };
        "Mod+Ctrl+F".center-column = { };

        "Mod+Tab".toggle-overview = { };

        "Mod+Shift+R".spawn = "screenrec-toggle";

        "Mod+N".spawn = [
          "makoctl"
          "dismiss"
        ];
        "Mod+Shift+N".spawn = [
          "makoctl"
          "dismiss"
          "-a"
        ];
        "Mod+Ctrl+N".spawn = [
          "makoctl"
          "restore"
        ];
        "Mod+Alt+N".spawn = [
          "makoctl"
          "mode"
          "-t"
          "do-not-disturb"
        ];

        # kanshi extends onto an external monitor by default; these switch by hand.
        "Mod+Ctrl+D".spawn = [
          "kanshictl"
          "switch"
          "docked"
        ];
        "Mod+Ctrl+E".spawn = [
          "kanshictl"
          "switch"
          "dual"
        ];

        "Mod+Escape".spawn = "hyprlock";
        # Quitting niri is the menu's "Log out" row.
        "Mod+Shift+E".spawn = "power-menu";
        "Mod+Shift+O".power-off-monitors = { };
      }
      // lib.mapAttrs (_: args: { spawn = [ "swayosd-client" ] ++ args; }) {
        XF86AudioRaiseVolume = [
          "--output-volume"
          "raise"
        ];
        XF86AudioLowerVolume = [
          "--output-volume"
          "lower"
        ];
        XF86AudioMute = [
          "--output-volume"
          "mute-toggle"
        ];
        XF86AudioMicMute = [
          "--input-volume"
          "mute-toggle"
        ];
        XF86MonBrightnessUp = [
          "--brightness"
          "raise"
        ];
        XF86MonBrightnessDown = [
          "--brightness"
          "lower"
        ];
      }
      //
        lib.mapAttrs
          (_: action: {
            spawn = [
              "playerctl"
              action
            ];
          })
          {
            XF86AudioPlay = "play-pause";
            XF86AudioPause = "play-pause";
            XF86AudioNext = "next";
            XF86AudioPrev = "previous";
          }
      // lib.listToAttrs (
        lib.concatMap (ws: [
          (lib.nameValuePair "Mod+${workspaceKey ws}" { focus-workspace = ws; })
          (lib.nameValuePair "Mod+Shift+${workspaceKey ws}" { move-column-to-workspace = ws; })
        ]) workspaces
      );

      _children =
        map (ws: { workspace._args = [ ws ]; }) workspaces
        ++ [
          {
            output = {
              _args = [ "eDP-1" ];
              scale = 1.0;
            };
          }
          { spawn-at-startup = [ "wallpaper-start" ]; }
          {
            window-rule = {
              geometry-corner-radius = 16;
              clip-to-geometry = true;
            };
          }
          {
            window-rule._children = [
              { match._props.is-focused = false; }
              { opacity = 0.9; }
            ];
          }
          {
            window-rule._children = [
              { match._props.app-id = "^pavucontrol$"; }
              { open-floating = true; }
            ];
          }
          {
            # niri already floats windows that have a parent; the title match catches file
            # choosers opened without one (no parent handle passed to the portal), from any app.
            window-rule._children = [
              # GTK 3 may report either its GApplication id or its program name.
              { match._props.app-id = "^(nm-connection-editor|org[.]gnome[.]nm_connection_editor)$"; }
              # Started as `kitty --class bluetui` from the rail's network chip.
              { match._props.app-id = "^bluetui$"; }
              { match._props.app-id = "^com[.]gabm[.]satty$"; }
              { match._props.app-id = "^gay[.]vaskel[.]soteria$"; }
              { match._props.app-id = "^xdg-desktop-portal-gtk$"; }
              { match._props.title = "^((Open|Save|Select|Choose) (Files?|Folders?|As)|File Upload)"; }
              { open-floating = true; }
            ];
          }
          {
            window-rule._children = [
              { match._props.app-id = "^FloatingEmacs$"; }
              { open-floating = true; }
              { default-column-width.proportion = 0.8; }
              { default-window-height.proportion = 0.8; }
            ];
          }
        ]
        # A layer shadow follows the whole surface, transparent margins included, so these
        # namespaces draw their shape edge to edge.
        ++
          lib.mapAttrsToList
            (namespace: radius: {
              layer-rule._children = [
                { match._props.namespace = "^${namespace}$"; }
                { shadow.on = { }; }
                { geometry-corner-radius = radius; }
              ];
            })
            {
              waybar = 18;
              notifications = 14;
              swayosd = 25;
              launcher = 16;
            }
        ++ lib.concatLists (
          lib.mapAttrsToList (
            ws: appIds:
            map (appId: {
              window-rule._children = [
                { match._props.app-id = appId; }
                { open-on-workspace = ws; }
              ];
            }) appIds
          ) workspaceApps
        );
    };
  };

  home.packages = with pkgs; [
    scratchpadCommit
    screenrecToggle
    wl-clipboard
    grim
    slurp
    satty
    wl-screenrec
    brightnessctl
    playerctl
    libnotify
  ];
}
