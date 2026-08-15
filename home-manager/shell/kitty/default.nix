{
  programs.kitty.enable = true;
  programs.kitty.themeFile = "Dracula";

  programs.kitty.font.name = "HackGen Console NF";
  programs.kitty.font.size = 13;

  # HackGen Console NF has no emoji glyphs; route emoji/pictograph/flag
  # ranges to Noto Color Emoji instead of falling back to a tofu glyph.
  programs.kitty.settings.symbol_map = "U+1F300-U+1FAFF,U+2600-U+27BF,U+1F1E6-U+1F1FF Noto Color Emoji";

  programs.kitty.settings = {
    remember_window_size = false;
    initial_window_width = 800;
    initial_window_height = 600;

    # OSC 52 clipboard support for tmux integration
    clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
    hide_window_decorations = true;

    macos_option_as_alt = "both";

    linux_display_server = "wayland";
    wayland_titlebar_color = "background";

    repaint_delay = 10;
    input_delay = 3;
    sync_to_monitor = true;

    cursor_shape = "beam";
    cursor_blink_interval = 0;

    scrollback_lines = 10000;

    mouse_hide_wait = 3.0;
    copy_on_select = "clipboard";

    enable_audio_bell = false;
    visual_bell_duration = 0;

    # Focus reporting (disable to prevent OI escape sequences in terminal apps)
    focus_reporting_protocol = "none";

    # Splits/Windows (not covered by Dracula theme)
    active_border_color = "#f8f8f2";
    inactive_border_color = "#6272a4";
  };

  programs.kitty.keybindings = {
    "ctrl+k" = "scroll_line_up";
    "ctrl+j" = "scroll_line_down";
    "ctrl+u" = "scroll_page_up";
    "ctrl+d" = "scroll_page_down";
    "ctrl+shift+g" = "scroll_end";
    "ctrl+g" = "scroll_home";

    "ctrl+shift+c" = "copy_to_clipboard";
    "ctrl+shift+v" = "paste_from_clipboard";

    "ctrl+plus" = "change_font_size all +1.0";
    "ctrl+minus" = "change_font_size all -1.0";
    "ctrl+0" = "change_font_size all 0";

    "ctrl+shift+f" = "show_scrollback";

    "ctrl+shift+n" = "new_os_window";
  };
}
