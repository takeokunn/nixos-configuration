{
  pkgs,
  llmAgentsPkgs,
  artoPkg,
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
  llmAgentsPkgs.codex

  # for web service
  discord
  slack
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

  # custom macOS apps
  artoPkg
]
