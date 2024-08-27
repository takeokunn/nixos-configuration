{ lib, pkgs, wezterm-flake, emacs-flake, neovim-nightly-overlay }:
let
  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  emacs-flake = import ./emacs-flake { inherit lib pkgs emacs-flake; };
  git = import ./git;
  nix-index = import ./nix-index;
  neovim = import ./neovim { inherit pkgs neovim-nightly-overlay; };
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
  emacs-flake
  git
  nix-index
  neovim
  roswell
  wezterm

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
