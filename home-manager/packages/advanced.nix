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
]
