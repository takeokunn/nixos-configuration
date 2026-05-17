{ nurPkgs }:
{
  programs.bat.enable = true;
  programs.bat.config.theme = "dracula";
  programs.bat.themes.dracula.src = "${nurPkgs.dracula-sublime}/share/sublime";
  programs.bat.themes.dracula.file = "Dracula.tmTheme";
  programs.bat.syntaxes.gleam.src = "${nurPkgs.sublime-gleam}/share/sublime";
  programs.bat.syntaxes.gleam.file = "syntax/gleam.sublime-syntax";
  programs.bat.syntaxes.justfile.src = "${nurPkgs.sublime-justfile}/share/sublime";
  programs.bat.syntaxes.justfile.file = "Syntax/Just.sublime-syntax";

  programs.fish.shellAliases.cat = "bat";
}
