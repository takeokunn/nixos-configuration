{ pkgs }: {
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
        src = builtins.fetchGit {
          url = "https://github.com/0rax/fish-bd";
          rev = "ab686e028bfe95fa561a4f4e57840e36902d4d7d";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchGit {
          url = "https://github.com/adriaanzon/fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchGit {
          url = "https://github.com/adriaanzon/fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchGit {
          url = "https://github.com/adriaanzon/fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
        };
      }
      {
        name = "fish-ghq";
        src = builtins.fetchGit {
          url = "https://github.com/decors/fish-ghq";
          rev = "cafaaabe63c124bf0714f89ec715cfe9ece87fa2";
        };
      }
      {
        name = "dracula";
        src = builtins.fetchGit {
          url = "https://github.com/dracula/fish";
          rev = "269cd7d76d5104fdc2721db7b8848f6224bdf554";
        };
      }
      {
        name = "done";
        src = builtins.fetchGit {
          url = "https://github.com/franciscolourenco/done";
          rev = "eb32ade85c0f2c68cbfcff3036756bbf27a4f366";
        };
      }
      {
        name = "autopair";
        src = builtins.fetchGit {
          url = "https://github.com/jorgebucaran/autopair.fish";
          rev = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
        };
      }
      {
        name = "nix-completions";
        src = builtins.fetchGit {
          url = "https://github.com/kidonng/nix-completions.fish";
          rev = "cd8a43bed96e0acc02228bc77502be8ba5fa0548";
        };
      }
      {
        name = "nix-env";
        src = builtins.fetchGit {
          url = "https://github.com/lilyball/nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
        };
      }
      {
        name = "fish-dart-completions";
        src = builtins.fetchGit {
          url = "https://github.com/takeokunn/fish-dart-completions";
          rev = "f52734d3bbb79f362aa6541b490f74df49f124ff";
        };
      }
    ];
  };
}
