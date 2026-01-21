{ pkgs }:
{
  programs.kitty = {
    enable = true;

    shellIntegration = {
      enableFishIntegration = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };

    settings = {
      font_size = 13.0;
      font_family = "HackGen Console NF";

      # OSC 52 clipboard support for tmux integration
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
      hide_window_decorations = true;

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

      # thanks. https://github.com/dracula/kitty/blob/master/dracula.conf
      foreground = "#f8f8f2";
      background = "#282a36";
      selection_foreground = "#ffffff";
      selection_background = "#44475a";
      url_color = "#8be9fd";

      # black
      color0 = "#21222c";
      color8 = "#6272a4";

      # red
      color1 = "#ff5555";
      color9 = "#ff6e6e";

      # green
      color2 = "#50fa7b";
      color10 = "#69ff94";

      # yellow
      color3 = "#f1fa8c";
      color11 = "#ffffa5";

      # blue
      color4 = "#bd93f9";
      color12 = "#d6acff";

      # magenta
      color5 = "#ff79c6";
      color13 = "#ff92df";

      # cyan
      color6 = "#8be9fd";
      color14 = "#a4ffff";

      # white
      color7 = "#f8f8f2";
      color15 = "#ffffff";

      # Cursor colors
      cursor = "#f8f8f2";
      cursor_text_color = "background";

      # Tab bar colors
      active_tab_foreground = "#282a36";
      active_tab_background = "#f8f8f2";
      inactive_tab_foreground = "#282a36";
      inactive_tab_background = "#6272a4";

      # Marks
      mark1_foreground = "#282a36";
      mark1_background = "#ff5555";

      # Splits/Windows
      active_border_color = "#f8f8f2";
      inactive_border_color = "#6272a4";
    };

    keybindings = {
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
  };
}
