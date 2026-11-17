{ inputs, pkgs, ... }:
# Activation text below must go into one of `preActivation`, `extraActivation` or `postActivation`,
# never an attribute named after the module. nix-darwin splices a fixed, enumerated list of slots
# into the script it runs, so any other name defines an option that typechecks, evaluates, and is
# never executed, with nothing reporting the omission.
#
# `text` is `types.lines`, so several modules sharing a slot concatenate rather than collide, and
# the whole script is one `set -e` sequence across every slot. Picking a later slot therefore adds
# failures that can abort you, it does not decouple you from anything; `postActivation` already
# carries mac-app-util and the entire home-manager activation. Prefer `extraActivation`, which runs
# near the front. A failure anywhere fails the switch loudly, which is the opposite of the silent
# no-op an unrecognised slot name produces.
{
  _module.args.guardAndGuide =
    inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;

  imports = [
    ./config/claude-code-settings.nix
    ./config/codex-settings.nix
    ./config/fonts.nix
    ./config/homebrew.nix
    ./config/networking.nix
    ./config/nix.nix
    ./config/security.nix
    ./config/services
    ./config/stylix.nix
    ./config/system.nix
    ./config/wallpaper.nix
  ];
}
