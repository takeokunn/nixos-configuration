let
  caches = import ../../shared/nix-caches.nix;
in
{
  nix.settings = {
    cores = 2;
    max-jobs = 6;
    allowed-users = [ "@wheel" ];
    substituters = [ "https://cache.nixos.org" ] ++ caches.substituters;
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ]
    ++ caches.trusted-public-keys;
    keep-outputs = true;
    keep-derivations = true;
    connect-timeout = 5;
    warn-dirty = false;
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
  nix.optimise.automatic = true;
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
}
