{
  config,
  options,
  pkgs,
  ...
}:
let
  d = import ../../../shared/dracula-roles.nix { inherit config options pkgs; };
in
{
  programs.fzf.enable = true;
  programs.fzf.tmux.enableShellIntegration = true;
  programs.fzf.defaultCommand = "fd --type f --hidden --exclude .git";
  programs.fzf.fileWidget.command = "fd --type f --hidden --exclude .git";
  programs.fzf.changeDirWidget.command = "fd --type d --hidden --exclude .git";
  programs.fzf.fileWidget.options = [ "--preview 'bat --color=always --style=plain {}'" ];
  programs.fzf.changeDirWidget.options = [ "--preview 'eza --tree --level=2 --color=always {}'" ];
  programs.fzf.defaultOptions = [ "--highlight-line" ];
  # Dracula's fzf color assignments with the Stylix palette.
  programs.fzf.colors = {
    fg = "#${d.foreground}";
    "fg+" = "#${d.foreground}";
    bg = "#${d.background}";
    "bg+" = "#${d.selection}";
    hl = "#${d.purple}";
    "hl+" = "#${d.purple}";
    info = "#${d.orange}";
    prompt = "#${d.green}";
    pointer = "#${d.pink}";
    marker = "#${d.pink}";
    spinner = "#${d.orange}";
    header = "#${d.comment}";
  };
}
