{
  programs.zellij = {
    enable = false;

    enableZshIntegration = true;
    enableFishIntegration = true;
    enableBashIntegration = true;

    rawConfig = ''
      default_layout "compact"
      default_shell "fish"
      hide_status_bar true
      mouse_mode false
      scroll_buffer_size 100000
      show_release_notes false
      show_startup_tips false
      simplified_ui true
      tab_bar false
      theme "dracula"

      keybinds {
          shared_except "locked" "normal" {
              bind "Ctrl [" { SwitchToMode "Normal"; }
          }

          locked {
              bind "Ctrl g" { SwitchToMode "Normal"; }
          }

          normal {
              bind "Ctrl [" { SwitchToMode "Locked"; }

              bind "p" { SwitchToMode "Pane"; }
              bind "t" { SwitchToMode "Tab"; }
              bind "s" { SwitchToMode "Scroll"; }
              bind "r" { SwitchToMode "Resize"; }
              bind "o" { SwitchToMode "Session"; }
          }

          pane {
              bind "h" "Left" { MoveFocus "Left"; }
              bind "l" "Right" { MoveFocus "Right"; }
              bind "j" "Down" { MoveFocus "Down"; }
              bind "k" "Up" { MoveFocus "Up"; }

              bind "d" { NewPane "Down"; SwitchToMode "Locked"; }
              bind "r" { NewPane "Right"; SwitchToMode "Locked"; }

              bind "f" { ToggleFocusFullscreen; SwitchToMode "Locked"; }
              bind "z" { TogglePaneFrames; SwitchToMode "Locked"; }
          }

          tab {
              bind "h" "Left" { GoToPreviousTab; }
              bind "l" "Right" { GoToNextTab; }

              bind "1" { GoToTab 1; SwitchToMode "Locked"; }
              bind "2" { GoToTab 2; SwitchToMode "Locked"; }
          }

          scroll {
              bind "j" "Down" { ScrollDown; }
              bind "k" "Up" { ScrollUp; }
              bind "d" { HalfPageScrollDown; }
              bind "u" { HalfPageScrollUp; }
          }
      }
    '';
  };
}
