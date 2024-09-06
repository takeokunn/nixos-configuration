{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [ docker-tramp plugins.consult-tramp ]
