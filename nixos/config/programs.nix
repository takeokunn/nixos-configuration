{ pkgs, ... }:
{
  programs = {
    fish.enable = true;
    noisetorch.enable = true;
    nix-ld.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    sway = {
      enable = true;
      xwayland.enable = true;
      wrapperFeatures.gtk = true;
    };
  };
}
