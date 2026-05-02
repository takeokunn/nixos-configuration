{
  programs.zoxide = {
    enable = true;
  };

  programs.fish = {
    shellAliases = {
      cd = "z";
    };
  };
}
