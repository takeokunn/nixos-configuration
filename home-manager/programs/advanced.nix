{ lib, pkgs, wezterm-flake, neovim-nightly-overlay, org-babel }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs org-babel; };
  git = import ./git;
  nix-index = import ./nix-index;
  neovim = import ./neovim { inherit pkgs neovim-nightly-overlay; };
  nyxt = import ./nyxt { inherit pkgs; };
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
