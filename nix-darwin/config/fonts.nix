{ pkgs, ... }:
{
  fonts.packages = import ../../shared/fonts.nix pkgs ++ [
    pkgs.monaspace
    pkgs.noto-fonts-emoji-blob-bin
    pkgs.noto-fonts-monochrome-emoji
  ];
}
