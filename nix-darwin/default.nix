{
  inputs,
  pkgs,
  username,
  emacsLib,
  nurPkgs,
  ...
}:
# Activation text below must go into `system.activationScripts.extraActivation`, never an
# attribute named after the module. nix-darwin splices a fixed, enumerated list of slots into the
# script it runs, so any other name defines an option that typechecks, evaluates, and is never
# executed, with nothing reporting the omission. `extraActivation` is one of the three slots
# nix-darwin opens to consumers, and its `text` is `types.lines`, so every module setting it is
# concatenated rather than overriding the others.
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
