{ pkgs }: {
  programs.bat = {
    enable = true;
    themes = {
      dracula = {
        src = pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "sublime";
          rev = "456d3289827964a6cb503a3b0a6448f4326f291b";
          sha256 = "1flajlii1w40g2yqs5iyfjvqxah1d83mvglsid2kn1mbajyshq7j";
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
          sha256 = "1fmxd95y15ay57zww9prkzd6kvx6bdlwrwk47law7rbb7xksi2gp";
        };
        file = "syntax/gleam.sublime-syntax";
      };
    };
  };

  programs.fish = {
    shellAliases = { cat = "bat"; };

    interactiveShellInit = ''
      set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"
    '';
  };
}
