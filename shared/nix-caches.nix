# Binary caches every host trusts and the settings that govern their use; each platform module adds its own on top.
{
  substituters = [
    "https://nix-community.cachix.org"
    "https://devenv.cachix.org"
    "https://takeokunn-nixos-configuration.cachix.org"
    "https://cache.numtide.com"
    "https://cclens.cachix.org"
  ];
  trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    "takeokunn-nixos-configuration.cachix.org-1:TzNrelquzPLqhPJnUFUp/RC/eHrtEegOjVvGEw+jsdc="
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    "cclens.cachix.org-1:0QUNU6PuVyf+yXOvg3n1rd3FksBoB3s3/Jty50iKRNQ="
  ];

  # Prefer a cached build wherever one exists, and keep what was fetched.
  settings = {
    always-allow-substitutes = true;
    builders-use-substitutes = true;
    # Build from source when a cache is unreachable instead of failing.
    fallback = true;
    connect-timeout = 5;
    http-connections = 50;
    max-substitution-jobs = 32;
    download-buffer-size = 268435456;
    # CI pushes each new closure to the cache; the default one-hour negative cache would hide it.
    narinfo-cache-negative-ttl = 300;
    # Garbage collection keeps build inputs, so a rebuild or a dev shell does not fetch them again.
    keep-outputs = true;
    keep-derivations = true;
  };
}
