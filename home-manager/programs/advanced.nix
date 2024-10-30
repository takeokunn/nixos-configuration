{ lib, pkgs, org-babel, sources }:
let
  emacs = import ./emacs { inherit pkgs org-babel sources; };
  git = import ./git;
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  offlineimap = import ./offlineimap { inherit pkgs; };
  rio = import ./rio { inherit pkgs; };

  # for window manager
  aerospace = import ./aerospace { inherit pkgs; };
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in [
  emacs
  git
  nix-index
  nyxt
  offlineimap
  rio

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
