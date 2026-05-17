{
  programs.kitty.enable = true;
  programs.kitty.themeFile = "Dracula";

  programs.kitty.font.name = "HackGen Console NF";
  programs.kitty.font.size = 13;

  programs.kitty.settings = {
    # Window size (don't remember previous size)
    remember_window_size = false;
    initial_window_width = 800;
    initial_window_height = 600;

    # OSC 52 clipboard support for tmux integration
    clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
    hide_window_decorations = true;

    # macOS: Option key as Meta
    macos_option_as_alt = "both";

    # Wayland settings (Linux only)
    linux_display_server = "wayland";
    wayland_titlebar_color = "background";

    # Performance
    repaint_delay = 10;
    input_delay = 3;
    sync_to_monitor = true;

    # Cursor
    cursor_shape = "beam";
    cursor_blink_interval = 0;

    # Scrollback
    scrollback_lines = 10000;

    # Mouse
    mouse_hide_wait = 3.0;
    copy_on_select = "clipboard";

    # Bell
    enable_audio_bell = false;
    visual_bell_duration = 0;

    # Focus reporting (disable to prevent OI escape sequences in terminal apps)
    focus_reporting_protocol = "none";

    # Splits/Windows (not covered by Dracula theme)
    active_border_color = "#f8f8f2";
    inactive_border_color = "#6272a4";
  };

  programs.kitty.keybindings = {
    # Vi-style scrolling
    "ctrl+k" = "scroll_line_up";
    "ctrl+j" = "scroll_line_down";
    "ctrl+u" = "scroll_page_up";
    "ctrl+d" = "scroll_page_down";
    "ctrl+shift+g" = "scroll_end";
    "ctrl+g" = "scroll_home";

    # Clipboard
    "ctrl+shift+c" = "copy_to_clipboard";
    "ctrl+shift+v" = "paste_from_clipboard";

    # Font size
    "ctrl+plus" = "change_font_size all +1.0";
    "ctrl+minus" = "change_font_size all -1.0";
    "ctrl+0" = "change_font_size all 0";

    # Search
    "ctrl+shift+f" = "show_scrollback";

    # New window
    "ctrl+shift+n" = "new_os_window";
  };
}
