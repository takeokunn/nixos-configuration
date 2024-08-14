{ config, lib, nixpkgs, pkgs, system, ... }: {
  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";
  environment.packages = with pkgs; [ git vim ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;
    config = import ../home-manager/minimum.nix { inherit system nixpkgs; };
  };
}
