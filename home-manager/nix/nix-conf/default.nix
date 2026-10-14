{ config, ... }:
{
  nix.settings = {
    # The token itself lives outside the repo (secret); this only points Nix
    # at where to find it.
    netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
    warn-dirty = false;
  };

  nix.extraOptions = ''
    !include ${config.home.homeDirectory}/.config/nix/work.conf
  '';
}
