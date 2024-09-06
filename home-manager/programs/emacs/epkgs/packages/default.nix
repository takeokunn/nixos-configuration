{ pkgs }:
epkgs:
let
  language = import ./packages/language.nix { inherit epkgs pkgs; };
  awesome = import ./packages/awesome.nix { inherit epkgs pkgs; };
  language_specific =
    import ./packages/language_specific.nix { inherit epkgs pkgs; };
  elfeed = import ./packages/elfeed.nix { inherit epkgs; };
  eshell = import ./packages/eshell.nix { inherit epkgs pkgs; };
  org_mode = import ./packages/org_mode.nix { inherit epkgs pkgs; };
  exwm = import ./packages/exwm { inherit epkgs; };
  ai = import ./packages/ai.nix { inherit epkgs pkgs; };
in language ++ awesome ++ language_specific ++ elfeed ++ eshell ++ org_mode
++ exwm ++ ai
