{
  programs.fzf.enable = true;
  programs.fzf.tmux.enableShellIntegration = true;
  programs.fzf.defaultCommand = "fd --type f --hidden --exclude .git";
  programs.fzf.fileWidget.command = "fd --type f --hidden --exclude .git";
  programs.fzf.changeDirWidget.command = "fd --type d --hidden --exclude .git";
  programs.fzf.fileWidget.options = [ "--preview 'bat --color=always --style=plain {}'" ];
  programs.fzf.changeDirWidget.options = [ "--preview 'eza --tree --level=2 --color=always {}'" ];
  programs.fzf.defaultOptions = [ "--highlight-line" ];
  programs.fzf.colors = {
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
}
