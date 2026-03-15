{ pkgs }:
pkgs.writeText "tui.json" (
  builtins.toJSON {
    "$schema" = "https://opencode.ai/tui.json";
    theme = "dracula";
    scroll_speed = 3;
    scroll_acceleration = {
      enabled = true;
    };
    diff_style = "auto";
    keybinds = {
      messages_half_page_down = "ctrl+d";
      messages_half_page_up = "ctrl+u";
      messages_next = "]";
      messages_previous = "[";
    };
  }
)
