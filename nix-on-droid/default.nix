{ config, lib, pkgs, ... }:
let minimum = import ../home-manager/minimum.nix { inherit pkgs; };
in {
  environment = {
    packages = minimum;
    etcBackupExtension = ".bak";
  };

  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
