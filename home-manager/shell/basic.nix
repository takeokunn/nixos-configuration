{ pkgs, ... }:
{
  imports = [
    ./bat
    ./bottom
    ./direnv
    ./dust
    ./eza
    ./fd
    ./fish
    ./fzf
    ./jq
    ./man
    ./readline
    ./ripgrep
    ./tmux
    ./wget
    ./zoxide
  ];

  home.packages = with pkgs; [
    dasel
    unixtools.watch
    nix-output-monitor
  ];
}
