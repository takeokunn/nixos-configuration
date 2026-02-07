{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
}:
with pkgs;
[
  # for util
  yq

  # for security
  sops
  yubikey-manager

  # for docker
  kubectl
  docker-client
  lazydocker

  # for ai
  llmAgentsPkgs.ccusage
  nurPkgs.z_ai-coding-helper

  # for web service
  discord
  slack
  nurPkgs.gogcli

  # for editor
  nurPkgs.kakehashi
]
++ lib.optionals stdenv.isLinux [
  # for desktop environment
  networkmanagerapplet
  networkmanager_dmenu
]
++ lib.optionals stdenv.isDarwin [
  # for macOS (brew-nix casks)
  brewCasks.keycastr
  brewCasks.raycast
  brewCasks.docker-desktop
  brewCasks.postman
  brewCasks.postico
  brewCasks.sequel-ace
  brewCasks.sublime-text
  brewCasks.ngrok
  brewCasks.clickup
  brewCasks.slite
  brewCasks.element
]
