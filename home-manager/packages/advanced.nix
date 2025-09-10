{ pkgs }:
with pkgs;
[
  # for lint
  similarity

  # for security
  sops
  yubikey-manager

  # for docker
  lazydocker

  # for web service
  discord
  slack
]
