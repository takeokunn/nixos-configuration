{ pkgs }:
{
  programs.tmux = {
    enable = true;
    terminal = "xterm-256color";
    baseIndex = 1;
    escapeTime = 1;
    historyLimit = 999999999;
    keyMode = "vi";
    prefix = "C-q";
    secureSocket = true;
    shell = "${pkgs.fish}/bin/fish";
    newSession = true;
    customPaneNavigationAndResize = true;
    resizeAmount = 5;
    disableConfirmationPrompt = true;

    plugins = with pkgs; [
      tmuxPlugins.open
      tmuxPlugins.sensible
      tmuxPlugins.urlview
      tmuxPlugins.pain-control
      tmuxPlugins.jump
      tmuxPlugins.resurrect
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
        '';
      }
      {
        plugin = tmuxPlugins.dracula;
        extraConfig = ''
          set -g clock-mode-colour "#81a2be"
          set-option -g @dracula-plugins "battery"
        '';
      }
    ];

    extraConfig = ''
      set-option -g default-command $SHELL
      set-option -g display-panes-time 15000
      set-option -g status-position top
      set-option -g status-interval 1

      # OSC 52 clipboard integration for proper UTF-8/Japanese text handling
      set -s set-clipboard on

      # Disable focus events to prevent OI escape sequences (overrides sensible plugin)
      set-option -g focus-events off

      bind-key C-g display-panes

      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"

      bind-key -T copy-mode-vi v     send-keys -X begin-selection
      bind-key -T copy-mode-vi y     send-keys -X copy-selection-and-cancel
      bind-key -T copy-mode-vi Enter send-keys -X copy-selection-and-cancel
    '';
  };
}
