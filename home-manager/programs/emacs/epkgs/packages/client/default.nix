{ epkgs, pkgs, sources }:
let plugins = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [
  # Client
  md4rd

  # Googling
  google-this
  google-translate

  # Mail
  mu4e
  mu4e-views
  plugins.mu4e-dashboard
]
