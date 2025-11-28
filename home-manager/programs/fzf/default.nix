{
  programs.fzf = {
    enable = true;

    enableFishIntegration = true;

    tmux.enableShellIntegration = true;

    defaultCommand = "fd --type f --hidden --exclude .git";
    fileWidgetCommand = "fd --type f --hidden --exclude .git";
    changeDirWidgetCommand = "fd --type d --hidden --exclude .git";

    fileWidgetOptions = [ "--preview 'bat --color=always --style=plain {}'" ];
    changeDirWidgetOptions = [ "--preview 'eza --tree --level=2 --color=always {}'" ];
    historyWidgetOptions = [ "--reverse" ];

    defaultOptions = [ "--highlight-line" ];

    colors = {
      fg = "#f8f8f2";
      "fg+" = "#f8f8f2";
      bg = "#282a36";
      "bg+" = "#44475a";
      hl = "#bd93f9";
      "hl+" = "#bd93f9";
      info = "#ffb86c";
      prompt = "#50fa7b";
      pointer = "#ff79c6";
      marker = "#ff79c6";
      spinner = "#ffb86c";
      header = "#6272a4";
    };
  };
}
