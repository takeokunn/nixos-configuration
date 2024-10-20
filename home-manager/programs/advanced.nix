{ lib, pkgs, org-babel, sources }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs org-babel sources; };
  git = import ./git;
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  rio = import ./rio { inherit pkgs; };
  roswell = import ./roswell { inherit pkgs; };

  # for window manager
  aerospace = import ./aerospace { inherit pkgs; };
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in [
  awscli
  emacs
  git
  nix-index
  nyxt
  rio
  roswell

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
