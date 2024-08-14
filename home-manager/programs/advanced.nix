{ pkgs }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm;
in [ awscli emacs git roswell wezterm ]
