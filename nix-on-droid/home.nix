{ config, lib, pkgs }: {
  home = {
    stateVersion = "24.11";
    packages = [ pkgs.hello ];
  };
}
