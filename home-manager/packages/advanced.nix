{ pkgs, nodePkgs }:
with pkgs;
[
  # for http
  xh

  # for lint
  similarity

  # for security
  sops
  yubikey-manager

  # for docker
  lazydocker

  # for ai
  # nodePkgs."@google/gemini-cli"
  nodePkgs."ccusage"

  # for web service
  discord
  slack
] ++ lib.optionals stdenv.isDarwin [
  # for macOS (brew-nix casks)
  brewCasks.aquaskk
  brewCasks.cleanshot
  brewCasks.keycastr
  brewCasks.raycast
  brewCasks.docker-desktop
  brewCasks.postman
  brewCasks.postico
  brewCasks.sequel-ace
  brewCasks.sublime-text
  brewCasks.ngrok
  brewCasks.clickup
]
