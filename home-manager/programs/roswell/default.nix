{ pkgs }: {
  home.packages = [ pkgs.roswell ];
  programs.fish = {
    interactiveShellInit = ''
      fish_add_path $HOME/.roswell/bin
    '';
  };
}
