{ lib, pkgs, wezterm-flake }:
let
  # ecspresso = pkgs.callPackage ../nixpkgs/ecspresso { };
  # mitamae = pkgs.callPackage ../nixpkgs/mitamae { };
  isucrud = pkgs.callPackage ../nixpkgs/isucrud { };

  awscli = import ./awscli;
  emacs = import ./emacs { inherit pkgs; };
  git = import ./git;
  nix-index = import ./nix-index;
  roswell = import ./roswell { inherit pkgs; };
  wezterm = import ./wezterm { inherit pkgs wezterm-flake; };

  # for window manager
  aerospace = import ./aerospace;
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit lib pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in [
  # ecspresso
  # mitamae
  isucrud

  awscli
  emacs
  git
  nix-index
  roswell
  wezterm

  # for window manager
  aerospace
  rofi
  sway
  swaylock
  waybar
]
