{
  imports = [
    ./nur.nix
    ./shell/basic.nix
    ./editor/basic.nix
    ./vcs/basic.nix
    ./security/basic.nix
    ./development/basic.nix
  ];

  home.stateVersion = "25.11";
  home.enableNixpkgsReleaseCheck = false;
}
