{ config, lib, pkgs, system, nixpkgs, emacs-overlay, ... }: {
  home = {
    stateVersion = "24.11";
    packages = import ../home-manager { inherit system nixpkgs emacs-overlay; };
  };
}
