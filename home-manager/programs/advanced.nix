{ lib, pkgs, wezterm-flake }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm { inherit pkgs wezterm-flake; };

  # for window manager
  aerospace = import ./aerospace;
  rofi = import ./rofi { inherit lib pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  mako = import ./mako;
  waybar = import ./waybar { inherit lib pkgs; };
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
  mako
  waybar
]
