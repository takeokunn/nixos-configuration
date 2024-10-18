{ lib, pkgs, neovim-nightly-overlay, org-babel, sources }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs org-babel sources; };
  git = import ./git;
  nix-index = import ./nix-index;
  neovim = import ./neovim { inherit pkgs neovim-nightly-overlay sources; };
  nyxt = import ./nyxt { inherit pkgs; };
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm { inherit pkgs; };

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
  neovim
  nyxt
  roswell
  wezterm

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
