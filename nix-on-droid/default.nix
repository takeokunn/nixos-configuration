{
  nixpkgs,
  pkgs,
  system,
  org-babel,
  emacs-overlay,
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
    config = import ../home-manager/basic.nix {
      inherit system nixpkgs;
      inherit org-babel emacs-overlay;
    };
  };
}
