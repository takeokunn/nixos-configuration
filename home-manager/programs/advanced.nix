{
  pkgs,
  nodePkgs,
  sources,
  org-babel,
  emacsPkg,
}:
let
  awscli = import ./awscli;
  aider = import ./aider;
  claude-code = import ./claude-code { inherit nodePkgs; };
  emacs = import ./emacs { inherit pkgs emacsPkg org-babel; };
  gh = import ./gh;
  gh-dash = import ./gh-dash;
  git = import ./git { inherit pkgs; };
  lnav = import ./lnav { inherit pkgs; };
  mu = import ./mu { inherit pkgs; };
  nix-index = import ./nix-index;
  offlineimap = import ./offlineimap;
  kitty = import ./kitty;
  sketchybar = import ./sketchybar;
  ssh = import ./ssh;
  copilot-language-server = import ./copilot-language-server { inherit pkgs nodePkgs; };
  nix-init = import ./nix-init;
  pandoc = import ./pandoc;

  # for window manager
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in
[
  awscli
  aider
  claude-code
  emacs
  gh
  gh-dash
  git
  lnav
  mu
  nix-index
  offlineimap
  kitty
  sketchybar
  ssh
  copilot-language-server
  nix-init
  pandoc

  # for window manager
  rofi
  sway
  swaylock
  waybar
]
