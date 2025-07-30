{
  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
    };

    gitCredentialHelper = {
      enable = true;
      hosts = [
        "https://github.com"
        "https://gist.github.com"
      ];
    };
  };

  programs.fish = {
    interactiveShellInit = ''
      eval (gh completion -s fish| source)

      # for nix
      set -x NIX_CONFIG "access-tokens = github.com="(gh auth token)
    '';
  };
}
