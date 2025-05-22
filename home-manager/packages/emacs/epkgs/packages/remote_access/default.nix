{
  epkgs,
  pkgs,
  sources,
}:
let
  packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in
[
  packages.consult-tramp
]
