{
  config,
  options,
  pkgs,
  nurPkgs,
  ...
}:
let
  d = import ../../../shared/dracula-roles.nix { inherit config options pkgs; };
in
{
  xdg.configFile."fish/completions/".source = ./completions;
  xdg.configFile."fish/completions/".recursive = true;

  xdg.configFile."fish/functions/".source = ./functions;
  xdg.configFile."fish/functions/".recursive = true;

  home.sessionVariables.COLORTERM = "truecolor";
  home.sessionVariables.LANG = "en_US.UTF-8";
  home.sessionVariables.LC_ALL = "en_US.UTF-8";

  programs.fish.enable = true;

  programs.fish.shellInit = ''
    set fish_greeting

    # macOS: fish launched from launchd may not inherit TMPDIR
    set -x TMPDIR /tmp

    ${
      if pkgs.stdenv.hostPlatform.isDarwin then
        "set -gx EMACS_SOCKET_NAME /tmp/emacs(id -u)/server"
      else
        "set -gx EMACS_SOCKET_NAME $XDG_RUNTIME_DIR/emacs/server"
    }

    # disable fzf Ctrl-R to use fish native history (avoids EINTR in tmux)
    set -gx FZF_CTRL_R_COMMAND ""

    # --global --path edits $PATH only when the directory is missing from it. The default mode
    # moves it to the front of the universal fish_user_paths even when present, which puts
    # /run/current-system/sw/bin/sudo (not setuid) ahead of NixOS's /run/wrappers/bin/sudo.
    if test -d /etc/profiles/per-user/(whoami)/bin
        fish_add_path --global --path /etc/profiles/per-user/(whoami)/bin
    end

    if test -d /run/current-system/sw/bin
        fish_add_path --global --path /run/current-system/sw/bin
    end

    # KITTY_INSTALLATION_DIR is set once by kitty at process launch and then
    # frozen into tmux's long-lived environment; a later kitty rebuild moves
    # the Nix store path and GC can remove the old one, so re-derive it from
    # the kitty currently on PATH before home-manager's fish integration
    # sources $KITTY_INSTALLATION_DIR/shell-integration/fish/vendor_conf.d.
    if set -q KITTY_INSTALLATION_DIR; and not test -d "$KITTY_INSTALLATION_DIR"
        set -l kitty_bin (command -v kitty)
        set -l candidate
        if test -n "$kitty_bin"
            set -l resolved (realpath "$kitty_bin")
            set -l root (string replace -r '/Applications/kitty\.app/Contents/MacOS/kitty$' "" $resolved)
            set candidate "$root/Applications/kitty.app/Contents/Resources/kitty"
        end
        if test -n "$candidate"; and test -d "$candidate"
            set -gx KITTY_INSTALLATION_DIR $candidate
        else
            set -e KITTY_INSTALLATION_DIR
        end
    end
  '';

  # dracula-fish's color assignments with the Stylix palette. Global scope so they shadow any
  # universal fish_color_* left behind by an earlier theme.
  programs.fish.interactiveShellInit = ''
    set -g fish_color_normal ${d.foreground}
    set -g fish_color_command ${d.cyan}
    set -g fish_color_keyword ${d.pink}
    set -g fish_color_quote ${d.yellow}
    set -g fish_color_redirection ${d.foreground}
    set -g fish_color_end ${d.orange}
    set -g fish_color_error ${d.red}
    set -g fish_color_param ${d.purple}
    set -g fish_color_option ${d.orange}
    set -g fish_color_comment ${d.comment}
    set -g fish_color_selection --background=${d.selection}
    set -g fish_color_search_match --background=${d.selection}
    set -g fish_color_match --background=${d.selection}
    set -g fish_color_operator ${d.green}
    set -g fish_color_escape ${d.pink}
    set -g fish_color_autosuggestion ${d.comment}
    set -g fish_color_cancel ${d.red} --reverse
    set -g fish_color_history_current --bold
    set -g fish_color_valid_path --underline
    set -g fish_color_status ${d.red}
    set -g fish_color_cwd ${d.green}
    set -g fish_color_cwd_root ${d.red}
    set -g fish_color_user ${d.cyan}
    set -g fish_color_host ${d.purple}
    set -g fish_color_host_remote ${d.purple}

    set -g fish_pager_color_progress ${d.comment}
    set -g fish_pager_color_background
    set -g fish_pager_color_prefix ${d.cyan}
    set -g fish_pager_color_completion ${d.foreground}
    set -g fish_pager_color_description ${d.comment}
    set -g fish_pager_color_selected_background --background=${d.selection}
    set -g fish_pager_color_selected_prefix ${d.cyan}
    set -g fish_pager_color_selected_completion ${d.foreground}
    set -g fish_pager_color_selected_description ${d.comment}
    set -g fish_pager_color_secondary_background
    set -g fish_pager_color_secondary_prefix ${d.cyan}
    set -g fish_pager_color_secondary_completion ${d.foreground}
    set -g fish_pager_color_secondary_description ${d.comment}
  '';

  programs.fish.shellInitLast = ''
    if test -e ~/.config/fish/private.fish
        source ~/.config/fish/private.fish
    end
  '';

  programs.fish.shellAliases.cdd = "cd ~/Desktop";
  programs.fish.shellAliases.make = "make -j8";
  programs.fish.shellAliases.magit = "emacsclient -nw --alternate-editor=\"\" -e \"(magit-status)\"";

  programs.fish.plugins =
    map
      (plugin: {
        name = plugin.pname;
        inherit (plugin) src;
      })
      [
        pkgs.fishPlugins.fish-bd
        nurPkgs.fish-artisan-completion
        nurPkgs.fish-ghq
        pkgs.fishPlugins.autopair
        nurPkgs.fish-nix-completions
        nurPkgs.fish-nix-env
        nurPkgs.fish-dart-completions
        nurPkgs.fish-by-binds-yourself
      ];
}
