{ config, ... }:
{
  # Declares $XDG_CONFIG_HOME/nix/nix.conf. Without this, `cachix use <name>`
  # writes a plain (non "extra-") `substituters`/`trusted-public-keys` pair
  # into that file, which *replaces* rather than appends to the substituters
  # nix-darwin/nixos already assembled in /etc/nix/nix.conf (Nix merges
  # settings file-by-file; a later plain key wins over everything before it,
  # while extra- keys append). That silently dropped numtide.cachix.org and
  # made llm-agents.nix packages like codex rebuild from source instead of
  # substituting. Keep every key here under extra- so it only ever adds to,
  # never replaces, the system-level substituters.
  #
  # nix.package is intentionally left unset: home-manager's nix-darwin/nixos
  # integration (nixos/common.nix) already forwards the OS-level nix.package
  # into this option, and setting it again here conflicts with that.
  nix.settings = {
    extra-substituters = [ "https://attmcojp.cachix.org" ];
    extra-trusted-public-keys = [
      "attmcojp.cachix.org-1:oru6oV4EttotACGO/YDhmsEyPlPSytG6zWUgTRH3BMQ="
    ];
    # The token itself lives outside the repo (secret); this only points Nix
    # at where to find it.
    netrc-file = "${config.home.homeDirectory}/.config/nix/netrc";
  };
}
