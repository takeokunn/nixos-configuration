{ pkgs }: {
  programs.sops = { enable = true; };

  programs.fish = {
    shellInit = ''
      # for private.fish
        if test -e ~/.config/fish/private.fish
            source ~/.config/fish/private.fish
        end
    '';
  };
}
