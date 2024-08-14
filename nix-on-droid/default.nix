{ config, lib, nixpkgs, pkgs, emacs-overlay, ... }: {
  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";
  environment.packages = with pkgs; [ git vim ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;

    config = { config, lib, pkgs, ... }: {
      home.stateVersion = "24.05";

      # insert home-manager config
    };
  };

}
