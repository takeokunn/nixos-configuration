{
  lib,
  options,
  nurPkgs,
  ...
}:
{
  programs.bat.enable = true;
  # Hosts with Stylix get the palette from it (stylix.targets.bat).
  programs.bat.config.theme = lib.mkIf (!(options ? stylix)) "dracula";
  programs.bat.themes.dracula = lib.mkIf (!(options ? stylix)) {
    src = "${nurPkgs.dracula-sublime}/share/sublime";
    file = "Dracula.tmTheme";
  };
  programs.bat.syntaxes.gleam.src = "${nurPkgs.sublime-gleam}/share/sublime";
  programs.bat.syntaxes.gleam.file = "syntax/gleam.sublime-syntax";
  programs.bat.syntaxes.justfile.src = "${nurPkgs.sublime-justfile}/share/sublime";
  programs.bat.syntaxes.justfile.file = "Syntax/Just.sublime-syntax";

  programs.fish.shellAliases.cat = "bat";
}
