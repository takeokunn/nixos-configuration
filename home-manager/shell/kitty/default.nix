{ config, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
  emojiFont = if pkgs.stdenv.hostPlatform.isDarwin then "Apple Color Emoji" else "Noto Color Emoji";
in
{
  programs.kitty.enable = true;
  # Palette and font come from Stylix (stylix.targets.kitty in shared/stylix-home-targets.nix).

  # The Stylix monospace font has no emoji glyphs, so emoji route to the platform's
  # color emoji font. In U+2300-U+23FF and U+2B00-U+2BFF only the emoji-flagged
  # codepoints are listed (e.g. U+23FA, Claude Code's record bullet): mapping the
  # full blocks would widen non-emoji glyphs such as U+2318 and U+23CE and break
  # column alignment.
  programs.kitty.settings.symbol_map = "U+1F300-U+1FAFF,U+2600-U+27BF,U+1F1E6-U+1F1FF,U+231A-U+231B,U+2328,U+23CF,U+23E9-U+23F3,U+23F8-U+23FA,U+2B05-U+2B07,U+2B1B-U+2B1C,U+2B50,U+2B55 ${emojiFont}";

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

    # Stylix's kitty theme leaves the split borders unset.
    active_border_color = colors.base05;
    inactive_border_color = colors.base03;
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
