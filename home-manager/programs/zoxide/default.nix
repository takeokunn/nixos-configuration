{
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.fish = {
    shellAliases = {
      cd = "z";
    };
  };
}
