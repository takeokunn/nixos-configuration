{
  programs.gh.enable = true;
  programs.gh.gitCredentialHelper.enable = true;

  programs.fish.interactiveShellInit = ''
    set -x NIX_CONFIG "access-tokens = github.com="(gh auth token)
  '';
}
