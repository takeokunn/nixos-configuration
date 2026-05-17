{ pkgs }:
let
  awscli = import ./awscli;
  k9s = import ./k9s;
in
[
  awscli
  k9s
  {
    home.packages = with pkgs; [
      kubectl
      docker-client
      lazydocker
    ];
  }
]
