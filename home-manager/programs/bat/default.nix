{ nurPkgs }:
{
  programs.bat = {
    enable = true;
    themes = {
      dracula = {
        src = "${nurPkgs.dracula-sublime}/share/sublime";
        file = "Dracula.tmTheme";
      };
    };
    syntaxes = {
      gleam = {
        src = "${nurPkgs.sublime-gleam}/share/sublime";
        file = "syntax/gleam.sublime-syntax";
      };
      justfile = {
        src = "${nurPkgs.sublime-justfile}/share/sublime";
        file = "Syntax/Just.sublime-syntax";
      };
    };
  };

  programs.fish = {
    shellAliases = {
      cat = "bat";
    };
  };
}
