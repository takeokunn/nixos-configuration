{ config, lib, pkgs }: {
  environment.etcBackupExtension = ".bak";
  system.stateVersion = "23.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  time.timeZone = "Asia/Tokyo";

  environment.packages = with pkgs; [ vim git hostname ];

  # home-manager = {
  #   config = ../../home-manager;
  #   backupFileExtension = "hm-bak";
  #   useGlobalPkgs = true;
  # };
}
