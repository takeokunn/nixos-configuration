{ pkgs, nurPkgs }:
# Note: Emacs socket path is defined in lib/emacs-constants.nix as single source of truth
# Fish uses (id -u) syntax while bash uses $(id -u), so we can't directly reference the constant
{
  xdg.configFile = {
    # completions
    "fish/completions/" = {
      source = ./completions;
      recursive = true;
    };

    # functions
    "fish/functions/" = {
      source = ./functions;
      recursive = true;
    };
  };

  programs.fish = {
    enable = true;

    shellInit = ''
      # for tmux
      if type -q tmux && test -z $TMUX
          tmux attach-session || tmux new-session
      end

      # suppress fish_greeting
      set fish_greeting

      # for env
      set -x TMPDIR /tmp
      set -x COLORTERM truecolor
      set -gx LANG en_US.UTF-8
      set -gx LC_ALL en_US.UTF-8

      # for emacs daemon socket (uses path from lib/emacs-constants.nix)
      # Note: fish uses (id -u) syntax, constants use $(id -u) for bash compatibility
      set -gx EMACS_SOCKET_NAME /tmp/emacs(id -u)/server

      # disable fzf Ctrl-R to use fish native history (avoids EINTR in tmux)
      set -gx FZF_CTRL_R_COMMAND ""

      if test -d /etc/profiles/per-user/(whoami)/bin
          fish_add_path /etc/profiles/per-user/(whoami)/bin
      end
    '';

    shellInitLast = ''
      # for private.fish
      if test -e ~/.config/fish/private.fish
          source ~/.config/fish/private.fish
      end
    '';

    shellAliases = {
      cdd = "cd ~/Desktop";
      make = "make -j8";
      magit = "emacsclient -nw --alternate-editor=\"\" -e \"(magit-status)\"";
    };

    plugins = [
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
  };
}
