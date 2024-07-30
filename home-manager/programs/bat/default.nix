{ pkgs }: {
  programs.bat = {
    enable = true;
    themes = {
      dracula = {
        src = pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "sublime";
          rev = "456d3289827964a6cb503a3b0a6448f4326f291b";
          sha256 = "019hfl4zbn4vm4154hh3bwk6hm7bdxbr1hdww83nabxwjn99ndhv";
        };
        file = "Dracula.tmTheme";
      };
    };
    syntaxes = {
      gleam = {
        src = pkgs.fetchFromGitHub {
          owner = "molnarmark";
          repo = "sublime-gleam";
          rev = "ff9638511e05b0aca236d63071c621977cffce38";
          sha256 = "94moZz9r5cMVPWTyzGlbpu9p2p/5Js7/KV6V4Etqvbo=";
        };
        file = "syntax/gleam.sublime-syntax";
      };
    };
  };

  programs.fish = {
    shellAliases = {
      cat = "bat";
    };

    interactiveShellInit = ''
      set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"
    '';
  };
}
