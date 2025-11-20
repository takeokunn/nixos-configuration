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
  # Client
  md4rd

  # Googling
  google-translate

  # Mail
  mu4e
  mu4e-views
  packages.mu4e-dashboard
]
