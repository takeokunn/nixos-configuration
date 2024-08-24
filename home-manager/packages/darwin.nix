{ pkgs }:
let mitamae = pkgs.callPackage ../nixpkgs/mitamae { };
in with pkgs; [ mitamae pinentry_mac terminal-notifier keycastr raycast ]
