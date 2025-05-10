{
  lib,
  pkgs,
  nodePkgs,
  sources,
  org-babel,
  emacsPkg,
}:
let
  emacs = import ./emacs { inherit pkgs emacsPkg org-babel; };
  gh = import ./gh;
  gh-dash = import ./gh-dash;
  git = import ./git { inherit pkgs; };
  lnav = import ./lnav { inherit pkgs; };
  mu = import ./mu { inherit pkgs; };
  nix-index = import ./nix-index;
  nyxt = import ./nyxt { inherit pkgs; };
  offlineimap = import ./offlineimap;
  kitty = import ./kitty;
  sketchybar = import ./sketchybar;
  ssh = import ./ssh;
  copilot-language-server = import ./copilot-language-server { inherit pkgs nodePkgs; };
  vscode = import ./vscode { inherit pkgs; };
  nix-init = import ./nix-init;
  pandoc = import ./pandoc;

  # for window manager
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
  nix-index
  nyxt
  offlineimap
  kitty
  sketchybar
  ssh
  copilot-language-server
  vscode
  nix-init
  pandoc

  # for window manager
  rofi
  sway
  swaylock
  waybar
]
