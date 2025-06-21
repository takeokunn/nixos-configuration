{
  programs.zellij = {
    enable = false;
    attachExistingSession = true;
    exitShellOnExit = true;
    enableFishIntegration = true;
    settings = {
      theme = "dracula";
      default_layout = "compact";
      simplified_ui = true;
      default_shell = "fish";
      mouse_mode = false;
      scroll_buffer_size = 100000;
      show_startup_tips = false;
      show_release_notes = false;
      tab_bar = false;
      hide_status_bar = true;
      keybinds = {
        normal = {
          h = "MoveFocusLeft";
          j = "MoveFocusDown";
          k = "MoveFocusUp";
          l = "MoveFocusRight";
          "Ctrl-w h" = "MoveFocusLeft";
          "Ctrl-w j" = "MoveFocusDown";
          "Ctrl-w k" = "MoveFocusUp";
          "Ctrl-w l" = "MoveFocusRight";
          "Ctrl-w s" = "NewPaneDown";
          "Ctrl-w v" = "NewPaneRight";
          "Ctrl-w q" = "CloseFocus";
          "g t" = "GoToNextTab";
          "g T" = "GoToPreviousTab";
          "Alt-h" = "ResizeLeft";
          "Alt-j" = "ResizeDown";
          "Alt-k" = "ResizeUp";
          "Alt-l" = "ResizeRight";
          "Ctrl-w d" = "Detach";
        };
      };
    };
  };
}
