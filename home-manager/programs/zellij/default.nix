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
    };
  };
}
