{
  programs.awscli.enable = false;

  programs.fish = {
    interactiveShellInit = ''
      set -x AWS_SDK_LOAD_CONFIG 1
    '';
  };
}
