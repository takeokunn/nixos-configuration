{ pkgs, ... }:
{
  imports = [
    ./modules/git-hooks
    ./tig
  ];

  home.packages = [ pkgs.ghq ];
}
