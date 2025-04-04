{
  programs.readline = {
    enable = true;
    extraConfig = ''
      # [prompt]
      set show-mode-in-prompt on
      set vi-ins-mode-string \1\e[6 q\2
      set vi-cmd-mode-string \1\e[2 q\2

      # [color]
      set colored-stats on
      set visible-stats on
      set mark-symlinked-directories on
      set colored-completion-prefix on
      set menu-complete-display-prefix on
    '';
  };
}
