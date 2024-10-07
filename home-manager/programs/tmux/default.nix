{ pkgs }: {
  programs.tmux = {
    enable = true;
    terminal = "xterm-256color";
    baseIndex = 1;
    escapeTime = 1;
    historyLimit = 999999999;
    keyMode = "vi";
    prefix = "C-q";
    plugins = with pkgs; [
      tmuxPlugins.open
      tmuxPlugins.resurrect
      tmuxPlugins.sensible
      tmuxPlugins.urlview
      tmuxPlugins.pain-control
      tmuxPlugins.jump
      {
        plugin = tmuxPlugins.dracula;
        extraConfig = ''
          set -g clock-mode-colour "#81a2be"
          set-option -g @dracula-plugins "battery"
        '';
      }
    ];

    extraConfig = ''
      set -g pane-base-index 1
      set -g display-panes-time 15000
      set -g default-command $SHELL

      set-option -g status-position top
      set-option -g status-right '[%Y-%m-%d(%a) %H:%M]'
      set-option -g status-interval 1
      set-option -g status-justify centre
      set-option -g status-bg "colour238"
      set-option -g status-fg "colour255"

      bind-key C-g display-panes

      bind | split-window -h -c '#{pane_current_path}'
      bind - split-window -v -c '#{pane_current_path}'

      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"

      bind-key -T copy-mode-vi v     send-keys -X begin-selection
      bind-key -T copy-mode-vi y     send-keys -X copy-pipe "pbcopy"
      bind-key -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel "pbcopy"

      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5
    '';
  };
}
