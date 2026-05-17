{ pkgs }:
let
  ssh = import ./ssh;
  gpg-agent = import ./gpg-agent { inherit pkgs; };
in
[
  ssh
  gpg-agent
  {
    home.packages = with pkgs; [
      sops
      yubikey-manager
    ];
  }
]
