{ pkgs, nodePkgs }:
with pkgs;
[
  # for lint
  similarity

  # for security
  sops
  yubikey-manager

  # for docker
  lazydocker

  # for ai
  gemini-cli
  nodePkgs."ccusage"

  # for web service
  discord
  slack
]
