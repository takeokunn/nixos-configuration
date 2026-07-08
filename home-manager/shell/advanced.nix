{ pkgs, ... }:
{
  imports = [
    ./kitty
  ];

  home.packages = [ pkgs.yq ];
}
