{ pkgs, wezterm-flake }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm { inherit pkgs wezterm-flake; };

  # for window manager
  aerospace = import ./aerospace;
  rofi = import ./rofi;
  sway = import ./sway;
  swaylock = import ./swaylock;
  mako = import ./mako;
  waybar = import ./waybar;
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
