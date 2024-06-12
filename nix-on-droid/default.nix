{ config, lib, pkgs, ... }: {
  # environment.packages = import ../home-manager/packages/basic.nix pkgs;
  environment.packages = [ pkgs.hello ];
  environment.etcBackupExtension = ".bak";

  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
