{ pkgs }: {
  home.file.".config/peco/config.json".source = ./config.json;
  home.packages = [ pkgs.peco ];
}
