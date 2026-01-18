{ pkgs }:
{
  programs.foot = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "HackGen Console NF:size=11";
        dpi-aware = "yes";
        pad = "8x8";
      };

      scrollback = {
        lines = 10000;
        multiplier = 3.0;
      };

      cursor = {
        style = "beam";
        blink = "no";
      };

      mouse = {
        hide-when-typing = "yes";
      };

      # Vi-style scrollback keybindings
      key-bindings = {
        # Vi-style scrolling (Ctrl + j/k for line, u/d for half-page)
        scrollback-up-line = "Control+k";
        scrollback-down-line = "Control+j";
        scrollback-up-half-page = "Control+u";
        scrollback-down-half-page = "Control+d";
        scrollback-up-page = "Control+b";
        scrollback-down-page = "Control+f";
        scrollback-home = "Control+g";
        scrollback-end = "Control+Shift+g";

        # Clipboard
        clipboard-copy = "Control+Shift+c";
        clipboard-paste = "Control+Shift+v";

        # Font size
        font-increase = "Control+plus";
        font-decrease = "Control+minus";
        font-reset = "Control+0";

        # Search
        search-start = "Control+Shift+f";

        # Spawn new terminal
        spawn-terminal = "Control+Shift+n";
      };

      search-bindings = {
        find-prev = "Control+Shift+n";
        find-next = "Control+n";
        cursor-left = "Control+h Left";
        cursor-right = "Control+l Right";
        cancel = "Escape Control+c";
      };

      # Dracula color scheme
      colors = {
        foreground = "f8f8f2";
        background = "282a36";

        # Normal colors
        regular0 = "21222c"; # black
        regular1 = "ff5555"; # red
        regular2 = "50fa7b"; # green
        regular3 = "f1fa8c"; # yellow
        regular4 = "bd93f9"; # blue
        regular5 = "ff79c6"; # magenta
        regular6 = "8be9fd"; # cyan
        regular7 = "f8f8f2"; # white

        # Bright colors
        bright0 = "6272a4"; # bright black
        bright1 = "ff6e6e"; # bright red
        bright2 = "69ff94"; # bright green
        bright3 = "ffffa5"; # bright yellow
        bright4 = "d6acff"; # bright blue
        bright5 = "ff92df"; # bright magenta
        bright6 = "a4ffff"; # bright cyan
        bright7 = "ffffff"; # bright white

        # Selection
        selection-foreground = "f8f8f2";
        selection-background = "44475a";
      };
    };
  };
}
