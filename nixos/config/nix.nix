{ pkgs, ... }:
{
  nix = {
    settings.cores = 8;
    optimise.automatic = true;
  };
}
