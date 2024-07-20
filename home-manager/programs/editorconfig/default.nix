{ pkgs }: {
  home.file.".editorconfig".source = ./.editorconfig;
  home.packages = [ pkgs.editorconfig-core-c ];
}
