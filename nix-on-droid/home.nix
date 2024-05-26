{ config, lib, pkgs }: {
  home = {
    stateVersion = "23.11";
    packages = import ../home-manager/packages/basic.nix { inherit pkgs; };
  };
}
