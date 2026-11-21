{ pkgs, nurPkgs, ... }:
{
  xdg.configFile."fish/completions/".source = ./completions;
  xdg.configFile."fish/completions/".recursive = true;

  xdg.configFile."fish/functions/".source = ./functions;
  xdg.configFile."fish/functions/".recursive = true;

  home.packages = [ (pkgs.writeShellScriptBin "yq-go" ''exec ${pkgs.lib.getExe pkgs.yq-go} "$@"'') ];

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
        nurPkgs.dracula-fish
        pkgs.fishPlugins.autopair
        nurPkgs.fish-nix-completions
        nurPkgs.fish-nix-env
        nurPkgs.fish-dart-completions
        nurPkgs.fish-by-binds-yourself
      ];
}
