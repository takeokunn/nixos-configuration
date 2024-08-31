{ epkgs, pkgs }:
let
  copilot = pkgs.callPackage ./nixpkgs/copilot.nix { inherit epkgs; };
  llm = pkgs.callPackage ./nixpkgs/llm.nix { inherit epkgs; };
in with epkgs; [ copilot llm ellama ]
