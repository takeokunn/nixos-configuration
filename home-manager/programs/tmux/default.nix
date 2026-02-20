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
    newSession = false;
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
          # Powerline with rounded separators (pill style)
          set -g @dracula-show-powerline true
          set -g @dracula-show-left-sep ""
          set -g @dracula-show-right-sep ""
          set -g @dracula-transparent-powerline-bg true

          # Left icon: session name
          set -g @dracula-show-left-icon " #S"
          set -g @dracula-left-icon-padding 1

          # Plugins: git only (branch name)
          set -g @dracula-plugins "git"
          set -g @dracula-show-flags true
          set -g @dracula-show-empty-plugins false

          # Git widget (branch name only)
          set -g @dracula-git-disable-status true
          set -g @dracula-git-show-current-symbol ""
          set -g @dracula-git-show-diff-symbol ""
          set -g @dracula-git-no-repo-message ""
          set -g @dracula-git-show-remote-status false
          set -g @dracula-git-colors "light_purple dark_gray"

          # Purple-based palette (replace loud green/yellow)
          set -g @dracula-colors "green='#bd93f9' yellow='#bd93f9'"

          # Border contrast
          set -g @dracula-border-contrast true
        '';
      }
    ];

    extraConfig = ''
      set-option -g default-command $SHELL
      set-option -g display-panes-time 15000
      set-option -g status-position top
      set-option -g status-interval 1

      # OSC 52 clipboard integration for proper UTF-8/Japanese text handling
      set-option -s set-clipboard on

      # Disable focus events to prevent escape sequences (overrides sensible plugin)
      set-option -g focus-events off

      # OSC 10/11/12 passthrough for active pane only (prevents escape sequence leaks)
      set-option -g allow-passthrough all

      bind-key C-g display-panes

      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"

      bind-key -T copy-mode-vi v     send-keys -X begin-selection
      bind-key -T copy-mode-vi y     send-keys -X copy-selection-and-cancel
      bind-key -T copy-mode-vi Enter send-keys -X copy-selection-and-cancel
    '';
  };
}
