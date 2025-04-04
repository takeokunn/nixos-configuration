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
  packages.copilot
  packages.llm
  packages.plz-media-type
  packages.plz-event-source
  packages.copilot-chat
  ellama
  aidermacs
]
