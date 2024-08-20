{ lib, pkgs, wezterm-flake }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm { inherit pkgs wezterm-flake; };

  # for window manager
  aerospace = import ./aerospace;
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in [
  awscli
  emacs
  git
  roswell
  wezterm

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
