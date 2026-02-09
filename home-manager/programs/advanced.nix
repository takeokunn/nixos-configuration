{
  lib,
  pkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  org-babel,
  emacsPkg,
  emacsLib,
  firefox-addons,
}:
let
  awscli = import ./awscli;
  chromium = import ./chromium { inherit pkgs; };
  firefox = import ./firefox { inherit pkgs; };
  claude-code = import ./claude-code { inherit pkgs llmAgentsPkgs mcp-servers-nix; };
  agent-skills-config = import ./agent-skills {
    inherit anthropic-skills cloudflare-skills hashicorp-agent-skills;
  };
  emacs = import ./emacs {
    inherit
      lib
      pkgs
      emacsPkg
      org-babel
      ;
  };
  doggo = import ./doggo;
  gh = import ./gh;
  gh-dash = import ./gh-dash;
  git = import ./git;
  lnav = import ./lnav { inherit pkgs; };
  mu = import ./mu { inherit pkgs; };
  nix-index = import ./nix-index;
  offlineimap = import ./offlineimap;
  kitty = import ./kitty { inherit pkgs; };
  sketchybar = import ./sketchybar { inherit pkgs; };
  ssh = import ./ssh;
  copilot-language-server = import ./copilot-language-server { inherit pkgs llmAgentsPkgs; };
  nix-init = import ./nix-init;
  pandoc = import ./pandoc;
  k9s = import ./k9s;

  # Modern window manager (niri ecosystem)
  niri = import ./niri { inherit pkgs emacsLib; };
  fuzzel = import ./fuzzel { inherit pkgs; };
  networkmanager-dmenu = import ./networkmanager-dmenu { inherit pkgs; };
  yazi = import ./yazi { inherit pkgs; };
  clipse = import ./clipse { inherit pkgs; };
  swww = import ./swww { inherit pkgs; };
  waybar = import ./waybar { inherit pkgs; };
  # theming
  gtk = import ./gtk { inherit pkgs; };
  qt = import ./qt { inherit pkgs; };
  zen-browser = import ./zen-browser { inherit pkgs firefox-addons; };
in
[
  awscli
  chromium
  firefox
  zen-browser
  claude-code
  agent-skills-config
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

  # Modern window manager (niri ecosystem)
  niri
  fuzzel
  networkmanager-dmenu
  yazi
  clipse
  swww
  waybar

  # theming
  gtk
  qt
]
