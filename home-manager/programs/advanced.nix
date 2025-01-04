{
  lib,
  pkgs,
  org-babel,
  sources,
  emacsPkg,
}:
let
  emacs = import ./emacs { inherit pkgs emacsPkg org-babel; };
  gh = import ./gh;
  gh-dash = import ./gh-dash;
  git = import ./git { inherit pkgs; };
  lnav = import ./lnav { inherit pkgs; };
  mu = import ./mu;
  msmtp = import ./msmtp { inherit pkgs; };
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  offlineimap = import ./offlineimap;
  rio = import ./rio { inherit pkgs; };
  sketchybar = import ./sketchybar;
  ssh = import ./ssh;

  # for window manager
  aerospace = import ./aerospace { inherit pkgs; };
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in
[
  emacs
  gh
  gh-dash
  git
  lnav
  mu
  msmtp
  nix-index
  nyxt
  offlineimap
  rio
  sketchybar
  ssh

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
