{ pkgs }:
pkgs.lib.mkIf pkgs.stdenv.isLinux {
  xdg.configFile."niri/config.kdl".text = ''
    // Niri Configuration - Modern Scrolling Tiling Compositor
    // Vi-style keybindings (hjkl navigation)

    input {
        keyboard {
            xkb {
                layout "jp"
            }
        }

        touchpad {
            tap
            dwt  // disable-while-typing
            natural-scroll
            accel-speed 0.2
        }

        mouse {
            accel-speed 0.0
        }
    }

    // Native gesture support
    gestures {
        // Workspace switching with drag-and-drop
        dnd-edge-workspace-switch {
            trigger-height 50
            delay-ms 100
        }

        // Hot corners for overview toggle
        hot-corners {
            top-left
        }
    }

    output "eDP-1" {
        scale 1.0
    }

    layout {
        gaps 8
        center-focused-column "never"

        preset-column-widths {
            proportion 0.33333
            proportion 0.5
            proportion 0.66667
        }

        default-column-width { proportion 0.5; }

        focus-ring {
            width 2
            active-color "#bd93f9"
            inactive-color "#44475a"
        }

        border {
            off
        }

        shadow {
            on
            softness 30
            spread 5
            offset x=0 y=5
            draw-behind-window true
            color "#00000064"
            inactive-color "#00000040"
        }

        struts {
            left 0
            right 0
            top 0
            bottom 0
        }
    }

    spawn-at-startup "swww-daemon"
    spawn-at-startup "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
    spawn-at-startup "fcitx5" "-d"

    prefer-no-csd

    screenshot-path "~/Pictures/Screenshots/Screenshot_%Y-%m-%d_%H-%M-%S.png"

    animations {
        slowdown 0.8

        workspace-switch {
            spring damping-ratio=1.0 stiffness=1000 epsilon=0.001
        }

        window-open {
            duration-ms 150
            curve "ease-out-expo"
        }

        window-close {
            duration-ms 150
            curve "ease-out-quad"
        }

        horizontal-view-movement {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.001
        }

        window-movement {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.001
        }

        window-resize {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.001
        }

        config-notification-open-close {
            spring damping-ratio=0.6 stiffness=1000 epsilon=0.001
        }
    }

    window-rule {
        geometry-corner-radius 8
        clip-to-geometry true
    }

    window-rule {
        match app-id="clipse"
        open-floating true
    }

    window-rule {
        match app-id="pavucontrol"
        open-floating true
    }

    binds {
        // Terminal (kitty)
        Mod+Return { spawn "kitty"; }
        Mod+Shift+Return { spawn "kitty" "-e" "tmux"; }

        // Application Launcher
        Mod+D { spawn "fuzzel"; }

        // Clipboard History (clipse in kitty)
        Mod+V { spawn "kitty" "--class" "clipse" "-e" "clipse"; }

        // Screenshot
        Mod+P { screenshot; }
        Mod+Shift+P { screenshot-screen; }
        Mod+Ctrl+P { screenshot-window; }

        // Close Window
        Mod+Shift+Q { close-window; }

        // Vi-style Focus Navigation (hjkl)
        Mod+H { focus-column-left; }
        Mod+J { focus-window-down; }
        Mod+K { focus-window-up; }
        Mod+L { focus-column-right; }

        // Vi-style Window Movement (Shift + hjkl)
        Mod+Shift+H { move-column-left; }
        Mod+Shift+J { move-window-down; }
        Mod+Shift+K { move-window-up; }
        Mod+Shift+L { move-column-right; }

        // Vi-style Workspace Navigation (Ctrl + j/k)
        Mod+Ctrl+K { focus-workspace-up; }
        Mod+Ctrl+J { focus-workspace-down; }

        // Move Window to Workspace (Ctrl + Shift + j/k)
        Mod+Ctrl+Shift+K { move-column-to-workspace-up; }
        Mod+Ctrl+Shift+J { move-column-to-workspace-down; }

        // Monitor Focus (Alt + hjkl)
        Mod+Alt+H { focus-monitor-left; }
        Mod+Alt+L { focus-monitor-right; }
        Mod+Alt+K { focus-monitor-up; }
        Mod+Alt+J { focus-monitor-down; }

        // Move Window to Monitor
        Mod+Alt+Shift+H { move-column-to-monitor-left; }
        Mod+Alt+Shift+L { move-column-to-monitor-right; }
        Mod+Alt+Shift+K { move-column-to-monitor-up; }
        Mod+Alt+Shift+J { move-column-to-monitor-down; }

        // Workspace Number Keys
        Mod+1 { focus-workspace 1; }
        Mod+2 { focus-workspace 2; }
        Mod+3 { focus-workspace 3; }
        Mod+4 { focus-workspace 4; }
        Mod+5 { focus-workspace 5; }
        Mod+6 { focus-workspace 6; }
        Mod+7 { focus-workspace 7; }
        Mod+8 { focus-workspace 8; }
        Mod+9 { focus-workspace 9; }

        Mod+Shift+1 { move-column-to-workspace 1; }
        Mod+Shift+2 { move-column-to-workspace 2; }
        Mod+Shift+3 { move-column-to-workspace 3; }
        Mod+Shift+4 { move-column-to-workspace 4; }
        Mod+Shift+5 { move-column-to-workspace 5; }
        Mod+Shift+6 { move-column-to-workspace 6; }
        Mod+Shift+7 { move-column-to-workspace 7; }
        Mod+Shift+8 { move-column-to-workspace 8; }
        Mod+Shift+9 { move-column-to-workspace 9; }

        // Window Layout
        Mod+F { fullscreen-window; }
        Mod+Shift+Space { toggle-floating; }

        // Column Width Adjustment
        Mod+Minus { set-column-width "-10%"; }
        Mod+Equal { set-column-width "+10%"; }

        // Window Height Adjustment
        Mod+Shift+Minus { set-window-height "-10%"; }
        Mod+Shift+Equal { set-window-height "+10%"; }

        // Consume/Expel Windows (Tab columns)
        Mod+BracketLeft { consume-window-into-column; }
        Mod+BracketRight { expel-window-from-column; }

        // Column Presets
        Mod+R { switch-preset-column-width; }
        Mod+Alt+R { reset-window-height; }
        Mod+Ctrl+R { switch-preset-window-height; }

        // Maximize Column
        Mod+Shift+F { maximize-column; }
        Mod+Ctrl+F { center-column; }

        // Overview Mode
        Mod+Tab { toggle-overview; }

        // Volume Control (with swayosd)
        XF86AudioRaiseVolume { spawn "swayosd-client" "--output-volume" "raise"; }
        XF86AudioLowerVolume { spawn "swayosd-client" "--output-volume" "lower"; }
        XF86AudioMute { spawn "swayosd-client" "--output-volume" "mute-toggle"; }

        // Brightness Control (with swayosd)
        XF86MonBrightnessUp { spawn "swayosd-client" "--brightness" "raise"; }
        XF86MonBrightnessDown { spawn "swayosd-client" "--brightness" "lower"; }

        // Media Control
        XF86AudioPlay { spawn "playerctl" "play-pause"; }
        XF86AudioPause { spawn "playerctl" "play-pause"; }
        XF86AudioNext { spawn "playerctl" "next"; }
        XF86AudioPrev { spawn "playerctl" "previous"; }

        // Screen Recording
        Mod+Shift+R { spawn "wl-screenrec" "-f" "$HOME/Videos/Recordings/recording_$(date +%Y-%m-%d_%H-%M-%S).mp4"; }

        // Notifications (mako)
        Mod+N { spawn "makoctl" "dismiss"; }
        Mod+Shift+N { spawn "makoctl" "dismiss" "-a"; }
        Mod+Ctrl+N { spawn "makoctl" "restore"; }

        // Lock Screen
        Mod+Escape { spawn "hyprlock"; }

        // Quit Niri
        Mod+Shift+E { quit; }

        // Power Off Monitors
        Mod+Shift+O { power-off-monitors; }
    }

    // Hotkey Overlay
    hotkey-overlay {
        skip-at-startup
    }

    cursor {
        xcursor-theme "Adwaita"
        xcursor-size 24
    }

    environment {
        DISPLAY ":0"
        QT_QPA_PLATFORM "wayland"
        QT_WAYLAND_DISABLE_WINDOWDECORATION "1"
        GDK_BACKEND "wayland"
        MOZ_ENABLE_WAYLAND "1"
        XDG_CURRENT_DESKTOP "niri"
        XDG_SESSION_TYPE "wayland"
        XDG_SESSION_DESKTOP "niri"
        NIXOS_OZONE_WL "1"
    }
  '';

  home.packages = with pkgs; [
    niri
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
