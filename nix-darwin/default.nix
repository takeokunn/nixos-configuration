{
  inputs,
  pkgs,
  username,
  emacsLib,
  nurPkgs,
  ...
}:
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
#
# `spotlight.nix` and `security.nix` still use module-named attributes and therefore have never
# run: grep `mdutil` and `pmset` in /run/current-system/activate returns 0 while `osascript` from
# `wallpaper.nix` returns 1. Repointing them is a behaviour change, not a cleanup — it would
# newly disable and erase the Spotlight index on the next switch — so it wants its own decision.
let
  claudeCodeSettings = import ./config/claude-code-settings.nix {
    inherit pkgs username;
    guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };
  fonts = import ./config/fonts.nix { inherit pkgs; };
  homebrew = import ./config/homebrew.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  security = import ./config/security.nix { inherit username; };
  services = import ./config/services { inherit emacsLib; };
  spotlight = import ./config/spotlight.nix;
  system = import ./config/system.nix { inherit username; };
  wallpaper = import ./config/wallpaper.nix { inherit nurPkgs username; };
in
{
  imports = [
    claudeCodeSettings
    fonts
    homebrew
    networking
    nix
    security
    services
    spotlight
    system
    wallpaper
  ];
}
