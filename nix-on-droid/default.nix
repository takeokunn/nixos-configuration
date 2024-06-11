{ config, lib, pkgs, system, nixpkgs, emacs-overlay, ... }: {
  environment.etcBackupExtension = ".bak";

  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  home-manager = {
    config = ./home.nix;
    extraSpecialArgs = { inherit system nixpkgs emacs-overlay; };
  };
}
