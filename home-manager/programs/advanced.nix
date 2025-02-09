{
  lib,
  pkgs,
  sources,
}:
let
  gh = import ./gh;
  gh-dash = import ./gh-dash;
  git = import ./git { inherit pkgs; };
  lnav = import ./lnav { inherit pkgs; };
  mu = import ./mu;
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  offlineimap = import ./offlineimap;
  rio = import ./rio;
  kitty = import ./kitty;
  sketchybar = import ./sketchybar;
  ssh = import ./ssh;

  # for window manager
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in
[
  gh
  gh-dash
  git
  lnav
  mu
  nix-index
  nyxt
  offlineimap
  rio
  kitty
  sketchybar
  ssh

  # for window manager
  rofi
  sway
  swaylock
  waybar
]
