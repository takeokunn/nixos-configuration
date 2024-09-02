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
        src = builtins.fetchFromGitHub {
          owner = "0rax";
          repo = "fish-bd";
          rev = "ab686e028bfe95fa561a4f4e57840e36902d4d7d";
          hash = "sha256-GeWjoakXa0t2TsMC/wpLEmsSVGhHFhBVK3v9eyQdzv0=";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchFromGitHub {
          owner = "adriaanzon";
          repo = "fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
          hash = "sha256-+LKQVuWORJcyuL/YZ3B86hpbV4rbSkj41Y9qgwXZXu4=";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchFromGitHub {
          owner = "adriaanzon";
          repo = "fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
          hash = "sha256-+LKQVuWORJcyuL/YZ3B86hpbV4rbSkj41Y9qgwXZXu4=";
        };
      }
      {
        name = "fish-artisan-completion";
        src = builtins.fetchFromGitHub {
          owner = "adriaanzon";
          repo = "fish-artisan-completion";
          rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
          hash = "sha256-+LKQVuWORJcyuL/YZ3B86hpbV4rbSkj41Y9qgwXZXu4=";
        };
      }
      {
        name = "fish-ghq";
        src = builtins.fetchFromGitHub {
          owner = "decors";
          repo = "fish-ghq";
          rev = "cafaaabe63c124bf0714f89ec715cfe9ece87fa2";
          hash = "sha256-6b1zmjtemNLNPx4qsXtm27AbtjwIZWkzJAo21/aVZzM=";
        };
      }
      {
        name = "dracula";
        src = builtins.fetchFromGitHub {
          owner = "dracula";
          repo = "fish";
          rev = "269cd7d76d5104fdc2721db7b8848f6224bdf554";
          hash = "sha256-Hyq4EfSmWmxwCYhp3O8agr7VWFAflcUe8BUKh50fNfY=";
        };
      }
      {
        name = "done";
        src = builtins.fetchFromGitHub {
          owner = "franciscolourenco";
          repo = "done";
          rev = "eb32ade85c0f2c68cbfcff3036756bbf27a4f366";
          hash = "sha256-DMIRKRAVOn7YEnuAtz4hIxrU93ULxNoQhW6juxCoh4o=";
        };
      }
      {
        name = "autopair";
        src = builtins.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "autopair.fish";
          rev = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
          hash = "sha256-qt3t1iKRRNuiLWiVoiAYOu+9E7jsyECyIqZJ/oRIT1A=";
        };
      }
      {
        name = "nix-completions";
        src = builtins.fetchFromGitHub {
          owner = "kidonng";
          repo = "nix-completions.fish";
          rev = "cd8a43bed96e0acc02228bc77502be8ba5fa0548";
          hash = "sha256-spnLmde41qQt8uJZFwiH0igFuVqZ6SvkwdA9Kbe2yz8=";
        };
      }
      {
        name = "nix-env";
        src = builtins.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
          hash = "sha256-RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
        };
      }
      {
        name = "fish-dart-completions";
        src = builtins.fetchFromGitHub {
          owner = "takeokunn";
          repo = "fish-dart-completions";
          rev = "f52734d3bbb79f362aa6541b490f74df49f124ff";
          hash = "sha256-CSvMkY5ObtAowr+PsPtJxsWaTZENgP5HrUU/PUoMtOw=";
        };
      }
    ];
  };
}
