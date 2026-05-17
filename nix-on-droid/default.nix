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

  home-manager.backupFileExtension = "hm-bak";
  home-manager.useGlobalPkgs = true;
  home-manager.sharedModules = [ nixvim.homeModules.nixvim ];
  home-manager.extraSpecialArgs = {
    inherit
      nixpkgs
      system
      nur-packages
      nixvim
      ;
  };
  home-manager.config = import ../home-manager/basic.nix;
}
