{ config, lib, pkgs, ... }: {
  # Read the changelog before changing this value
  home.stateVersion = "23.11";

  # insert home-manager config

  home.packages =
    import ../../home-manager/packages/basic.nix { inherit pkgs; };
}
