{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gopls
    typescript-language-server
    rust-analyzer
    nixd
    bash-language-server
  ];
}
