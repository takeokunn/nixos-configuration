{ pkgs, wezterm-flake }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  roswell = import ./roswell { inherit pkgs; };
  # rofi = import ./rofi;
  # sway = import ./sway;
  wezterm = import ./wezterm { inherit pkgs wezterm-flake; };
in [ awscli emacs git roswell rofi sway wezterm ]
