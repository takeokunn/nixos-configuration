{
  programs.direnv = {
    enable = true;
    config.global.disable_stdin = true;
    stdlib = ''
      use_github_token() {
        export GITHUB_TOKEN=$(gh auth token)
      }
    '';
  };

  programs.fish = {
    interactiveShellInit = ''
      set -gx DIRENV_LOG_FORMAT ""
      eval (direnv hook fish)
    '';
  };
}
