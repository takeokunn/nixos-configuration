{ pkgs, inputs, ... }:
let
  requirements = import ../../shared/codex-managed-settings.nix {
    inherit pkgs;
    guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };
  requirementsFile = (pkgs.formats.toml { }).generate "codex-requirements.toml" requirements;
in
{
  # Use the same activation slot as Claude Code's managed policy, with a regular destination file.
  system.activationScripts.extraActivation.text = ''
    mkdir -p /etc/codex
    install -m 0644 ${requirementsFile} /etc/codex/requirements.toml
  '';
}
