{
  nixpkgs,
  pkgs,
  system,
  nur-packages,
  nixvim,
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
    sharedModules = [ nixvim.homeModules.nixvim ];
    extraSpecialArgs = {
      inherit nixpkgs system nur-packages nixvim;
    };
    config = import ../home-manager/basic.nix;
  };
}
