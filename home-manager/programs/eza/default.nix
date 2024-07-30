{
  programs.eza = {
    enable = true;
    git = true;
    icons = true;

    enableBashIntegration = false;
    enableZshIntegration = false;
    enableFishIntegration = false;
    enableIonIntegration = false;
  };

  programs.fish = {
    shellAliases = {
      ls = "eza";
      la = "eza -a";
      lt = "eza --tree";
      ll = "eza -la";
      lla = "eza -la";
    };
  };
}
