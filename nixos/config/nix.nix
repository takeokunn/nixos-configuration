let
  caches = import ../../shared/nix-caches.nix;
in
{
  nix.settings = caches.settings // {
    cores = 2;
    max-jobs = 6;
    allowed-users = [ "@wheel" ];
    substituters = [ "https://cache.nixos.org" ] ++ caches.substituters;
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ]
    ++ caches.trusted-public-keys;
    warn-dirty = false;
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
  # Builds give way to the desktop for CPU and disk.
  nix.daemonCPUSchedPolicy = "idle";
  nix.daemonIOSchedClass = "idle";

  # The NixOS manual is rebuilt whenever a module option changes; the online one serves instead.
  documentation.nixos.enable = false;

  nix.optimise.automatic = true;
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
}
