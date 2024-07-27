{
  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";
  environment.packages = [ pkgs.git ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;
  };
}
