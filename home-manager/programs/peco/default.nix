{ pkgs }:
{
  programs.peco = {
    enable = true;
    config = {
      Use256Color = true;
      StickySelection = true;
      Prompt = "[peco] >";
      Keymap = {
        "C-k" = "peco.KillEndOfLine";
        "M-v" = "peco.ScrollPageUp";
        "C-v" = "peco.ScrollPageDown";
        "C-x,C-c" = "peco.Cancel";
      };
    };
  };
}
