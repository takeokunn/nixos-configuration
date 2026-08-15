{ config, ... }:
{
  nix.settings = {
    netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
  };

  nix.extraOptions = ''
    !include ${config.home.homeDirectory}/.config/nix/work.conf
  '';
}
