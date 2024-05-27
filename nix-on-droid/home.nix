{ config, lib, pkgs, system, nixpkgs, emacs-overlay, ... }: {
  home = {
    stateVersion = "23.11";
    packages = import ../home-manager { inherit system nixpkgs emacs-overlay; };
  };
}
