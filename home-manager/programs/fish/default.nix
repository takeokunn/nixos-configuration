{ pkgs, sources }:
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
      if type -q tmux && test -z $TMUX && status --is-login
          tmux_attach_session_if_needed
      end

      # suppress fish_greeting
      set fish_greeting

      # for env
      set -x TMPDIR /tmp
      set -x COLORTERM truecolor

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
        name = sources.fish-bd.pname;
        src = sources.fish-bd.src;
      }
      {
        name = sources.fish-artisan-completion.pname;
        src = sources.fish-artisan-completion.src;
      }
      {
        name = sources.fish-ghq.pname;
        src = sources.fish-ghq.src;
      }
      {
        name = sources.dracula-fish.pname;
        src = sources.dracula-fish.src;
      }
      {
        name = sources.fish-done.pname;
        src = sources.fish-done.src;
      }
      {
        name = sources.fish-autopair.pname;
        src = sources.fish-autopair.src;
      }
      {
        name = sources.fish-nix-completions.pname;
        src = sources.fish-nix-completions.src;
      }
      {
        name = sources.fish-nix-env.pname;
        src = sources.fish-nix-env.src;
      }
      {
        name = sources.fish-dart-completions.pname;
        src = sources.fish-dart-completions.src;
      }
    ];
  };
}
