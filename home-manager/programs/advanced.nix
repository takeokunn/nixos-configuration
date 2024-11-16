{
  lib,
  pkgs,
  org-babel,
  sources,
}:
let
  emacs = import ./emacs { inherit pkgs org-babel sources; };
  git = import ./git { inherit pkgs; };
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  offlineimap = import ./offlineimap;
  rio = import ./rio { inherit pkgs; };
  ssh = import ./ssh;
  sketchybar = import ./sketchybar;

  # for window manager
  aerospace = import ./aerospace { inherit pkgs; };
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in
[
  emacs
  git
  nix-index
  nyxt
  offlineimap
  rio
  ssh
  sketchybar

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
