{ pkgs, sources }:
{
  programs.bat = {
    enable = true;
    themes = {
      dracula = {
        src = sources.dracula-sublime.src;
        file = "Dracula.tmTheme";
      };
    };
    syntaxes = {
      gleam = {
        src = sources.sublime-gleam.src;
        file = "syntax/gleam.sublime-syntax";
      };
    };
  };

  programs.fish = {
    shellAliases = {
      cat = "bat";
    };
  };
}
