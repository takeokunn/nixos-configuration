{ pkgs }: {
  home.packages = with pkgs; [ roswell ];

  programs.fish = {
    interactiveShellInit = ''
      fish_add_path $HOME/.roswell/bin
    '';
  };
}
