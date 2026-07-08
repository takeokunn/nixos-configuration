{ pkgs, ... }:
{
  imports = [
    ./ssh
    ./gpg-agent
  ];

  home.packages = with pkgs; [
    sops
    yubikey-manager
  ];
}
