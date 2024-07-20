{ config, lib, pkgs, ... }:
let
  # packages
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # programs
  programs = import ./programs;
in {
  imports = programs;

  home.packages = basicPkgs;
}
