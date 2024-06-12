{ config, lib, pkgs }: {
  environment.packages = import ../home-manager/packages/basic.nix pkgs;
  environment.etcBackupExtension = ".bak";

  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
