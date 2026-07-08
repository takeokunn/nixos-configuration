{ pkgs, ... }:
{
  imports = [
    ./awscli
    ./k9s
  ];

  home.packages = with pkgs; [
    kubectl
    docker-client
    lazydocker
  ];
}
