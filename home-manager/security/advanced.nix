{ pkgs, ... }:
{
  imports = [
    ./ssh
    ./gpg-agent
    ./secret-scan
  ];

  home.packages = with pkgs; [
    sops
    yubikey-manager
  ];
}
