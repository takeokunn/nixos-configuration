{
  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      eval (gh completion -s fish| source)

      # for nix
      set -x NIX_CONFIG "access-tokens = github.com="(gh auth token)
    '';
  };
}
