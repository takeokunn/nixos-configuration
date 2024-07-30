{
  programs.gh = {
    enable = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      eval (gh completion -s fish| source)
    '';
  };
}
