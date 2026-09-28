{ pkgs, nur-packages, ... }:
{
  # aitoolsPackage only reaches default.nix through nur-packages' flake outputs;
  # a bare import leaves nurPkgs.aitools null on every system.
  _module.args.nurPkgs = import nur-packages {
    inherit pkgs;
    aitoolsPackage = nur-packages.legacyPackages.${pkgs.stdenv.hostPlatform.system}.aitools or null;
  };
}
