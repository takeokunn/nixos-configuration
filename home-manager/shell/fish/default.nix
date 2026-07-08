{ pkgs, nurPkgs, ... }:
# Note: Emacs socket path is defined in lib/emacs-constants.nix as single source of truth
# Fish uses (id -u) syntax while bash uses $(id -u), so we can't directly reference the constant
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
    # for tmux
    if type -q tmux && test -z $TMUX
        tmux attach-session || tmux new-session
    end

    # suppress fish_greeting
    set fish_greeting

    # macOS: fish launched from launchd may not inherit TMPDIR
    set -x TMPDIR /tmp

    # for emacs daemon socket (uses path from lib/emacs-constants.nix)
    # Note: fish uses (id -u) syntax, constants use $(id -u) for bash compatibility
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
  '';

  programs.fish.shellInitLast = ''
    # for private.fish
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
