{ pkgs, inputs, ... }:
let
  requirements = import ../../shared/codex-managed-settings.nix {
    inherit pkgs;
    guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };
in
{
  environment.etc."codex/requirements.toml" = {
    source = (pkgs.formats.toml { }).generate "codex-requirements.toml" requirements;
    mode = "0644";
  };
}
