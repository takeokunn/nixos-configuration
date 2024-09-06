{ pkgs }:
let sources = pkgs.callPackage ../../../_sources/generated.nix { };
in {
  home.file = {
    # completions
    ".config/fish/completions/" = {
      source = ./completions;
      recursive = true;
    };

    # functions
    ".config/fish/functions/" = {
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

      # supress fish_greeting
      set fish_greeting

      # env
      set -x TMPDIR /tmp

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
    };

    plugins = [
      {
        name = "fish-bd";
        src = sources.fish-bd.src;
      }
      {
        name = "fish-artisan-completion";
        src = sources.fish-artisan-completion.src;
      }
      {
        name = "fish-ghq";
        src = sources.fish-ghq.src;
      }
      {
        name = "dracula";
        src = sources.dracula-fish.src;
      }
      {
        name = "done";
        src = sources.fish-done.src;
      }
      {
        name = "autopair";
        src = sources.fish-autopair;
      }
      {
        name = "nix-completions";
        src = sources.fish-nix-completions.src;
      }
      {
        name = "nix-env";
        src = sources.fish-nix-env.src;
      }
      {
        name = "fish-dart-completions";
        src = sources.fish-dart-completions.src;
      }
    ];
  };
}
