{
  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
      prompt = "enabled";
    };
  };

  programs.fish = {
    interactiveShellInit = ''
      eval (gh completion -s fish| source)
    '';
  };
}
