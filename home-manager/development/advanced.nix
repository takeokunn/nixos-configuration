{ pkgs, ... }:
{
  imports = [
    ./pandoc
    ./doggo
    ./lnav
  ];

  # Duplicate-code detection.
  home.packages = with pkgs; [
    similarity
  ];
}
