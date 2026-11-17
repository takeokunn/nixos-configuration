{ inputs, pkgs, ... }:
# Activation text in the imported modules must go into `preActivation`, `extraActivation` or
# `postActivation`, never an attribute named after the module: nix-darwin runs only that fixed list
# of slots, and any other name evaluates silently and never executes.
#
# `text` is `types.lines`, so modules sharing a slot concatenate, and all slots form one `set -e`
# script. A later slot isolates nothing (`postActivation` already carries mac-app-util and the
# home-manager activation), so prefer `extraActivation`, which runs near the front.
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
