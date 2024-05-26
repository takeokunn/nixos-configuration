{ config, lib, pkgs, emacs-overlay, ... }: {
  environment.etcBackupExtension = ".bak";

  system.stateVersion = "23.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  time.timeZone = "Asia/Tokyo";

  home-manager = {
    config = ./home.nix;
    extraSpecialArgs = { inherit emacs-overlay; };
  };
}
