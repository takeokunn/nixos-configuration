{
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.direnv.silent = true;
  programs.direnv.config.global.disable_stdin = true;
  programs.direnv.config.global.warn_timeout = "0";
  programs.direnv.stdlib = ''
    use_github_token() {
      export GITHUB_TOKEN=$(gh auth token)
    }

    use_github_token_as() {
      export GITHUB_TOKEN=$(gh auth token --user "$1")
    }
  '';
}
