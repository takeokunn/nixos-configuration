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
      if pkgs.stdenv.isDarwin then
        "set -gx EMACS_SOCKET_NAME /tmp/emacs(id -u)/server"
      else
        "set -gx EMACS_SOCKET_NAME $XDG_RUNTIME_DIR/emacs/server"
    }

    # disable fzf Ctrl-R to use fish native history (avoids EINTR in tmux)
    set -gx FZF_CTRL_R_COMMAND ""

    if test -d /etc/profiles/per-user/(whoami)/bin
        fish_add_path /etc/profiles/per-user/(whoami)/bin
    end

    if test -d /run/current-system/sw/bin
        fish_add_path /run/current-system/sw/bin
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

  programs.fish.plugins = [
    {
      name = pkgs.fishPlugins.fish-bd.pname;
      src = pkgs.fishPlugins.fish-bd.src;
    }
    {
      name = nurPkgs.fish-artisan-completion.pname;
      src = nurPkgs.fish-artisan-completion.src;
    }
    {
      name = nurPkgs.fish-ghq.pname;
      src = nurPkgs.fish-ghq.src;
    }
    {
      name = nurPkgs.dracula-fish.pname;
      src = nurPkgs.dracula-fish.src;
    }
    {
      name = pkgs.fishPlugins.autopair.pname;
      src = pkgs.fishPlugins.autopair.src;
    }
    {
      name = nurPkgs.fish-nix-completions.pname;
      src = nurPkgs.fish-nix-completions.src;
    }
    {
      name = nurPkgs.fish-nix-env.pname;
      src = nurPkgs.fish-nix-env.src;
    }
    {
      name = nurPkgs.fish-dart-completions.pname;
      src = nurPkgs.fish-dart-completions.src;
    }
    {
      name = nurPkgs.fish-by-binds-yourself.pname;
      src = nurPkgs.fish-by-binds-yourself.src;
    }
  ];
}
