{
  nixpkgs,
  pkgs,
  system,
  ...
}:
{
  environment.packages = with pkgs; [ git ];

  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";

  user.shell = "${pkgs.fish}/bin/fish";

  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;
    config = import ../home-manager/basic.nix { inherit system nixpkgs; };
  };
}
