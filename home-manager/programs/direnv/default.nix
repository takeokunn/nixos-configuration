{
  programs.direnv = {
    enable = true;
    config.global.disable_stdin = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      set -gx DIRENV_LOG_FORMAT ""
      eval (direnv hook fish)
    '';
  };
}
