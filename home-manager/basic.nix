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
  home.preferXdgDirectories = true;
  home.enableNixpkgsReleaseCheck = false;

  # The option reference is rebuilt whenever a module changes; the online one serves instead.
  manual.manpages.enable = false;
}
