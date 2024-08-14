{ pkgs }: {
  time.timeZone = "Asia/Tokyo";

  system.stateVersion = "24.05";
  environment.packages = with pkgs; [ git vim ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
