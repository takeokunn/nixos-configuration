{
  epkgs,
  pkgs,
  sources,
}:
let
  packages = pkgs.callPackage ./packages.nix { inherit sources epkgs; };
in
with epkgs;
[
  # copilot
  packages.copilot
  copilot-chat
  ellama
]
