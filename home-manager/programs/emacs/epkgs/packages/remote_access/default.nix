{ epkgs, pkgs, sources }:
let packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in with epkgs; [ docker-tramp packages.consult-tramp ]
