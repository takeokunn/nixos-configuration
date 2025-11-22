{
  pkgs,
  nodePkgs,
  mcp-servers-nix,
  sources,
  org-babel,
  emacsPkg,
}:
let
  awscli = import ./awscli;
  claude-code = import ./claude-code { inherit pkgs nodePkgs mcp-servers-nix; };
  codex = import ./codex { inherit pkgs nodePkgs mcp-servers-nix; };
  emacs = import ./emacs { inherit pkgs emacsPkg org-babel; };
  doggo = import ./doggo;
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
  k9s = import ./k9s;

  # for window manager
  rofi = import ./rofi { inherit pkgs; };
  sway = import ./sway;
  swaylock = import ./swaylock { inherit pkgs; };
  waybar = import ./waybar { inherit pkgs; };
in
[
  awscli
  claude-code
  codex
  emacs
  doggo
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
  k9s

  # for window manager
  rofi
  sway
  swaylock
  waybar
]
