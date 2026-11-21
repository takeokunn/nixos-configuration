{
  pkgs,
  username,
  guardAndGuide,
  ...
}:
let
  requirements = import ../../shared/codex-managed-settings.nix {
    inherit pkgs guardAndGuide;
    homeDirectory = "/home/${username}";
  };
in
{
  environment.etc."codex/requirements.toml" = {
    source = (pkgs.formats.toml { }).generate "codex-requirements.toml" requirements;
    mode = "0644";
  };
}
