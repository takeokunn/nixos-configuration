{ pkgs }: {
  home.file.".config/wgetrc".source = ./.wgetrc;
  home.packages = with pkgs; [ wget ];
}
