{ pkgs }: {
  home.file.".config/wgetrc".source = ./.wgetrc;
  home.packages = [ pkgs.wget ];
}
